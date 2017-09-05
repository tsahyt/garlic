{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.Model.Queries
(
    recipes,
    ingredientsFor,
    WeighedIngredient (..),
    fromIngredient,
    wingrAmount,
    wingrUnit,
    wingrIngr,
    wingrOptional,
    completionList,
    ingredientByName,

    -- * Updates
    newRecipe,
    updateRecipe,
    deleteRecipe,
    newIngredient,
)
where

import Control.Lens.TH
import Garlic.Data.Units
import Garlic.Model
import Garlic.Types
import Database.Esqueleto
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Text as T

import qualified Database.Persist as P

-- | Weighted ingredients are ingredients with an amount and a unit.
data WeighedIngredient = WeighedIngredient
    { _wingrAmount   :: Double
    , _wingrUnit     :: Unit
    , _wingrOptional :: Bool
    , _wingrIngr     :: Entity Ingredient
    }
    deriving Show

fromIngredient :: Entity Ingredient -> WeighedIngredient
fromIngredient e@(Entity _ v) = 
    WeighedIngredient 
        (ingredientBasicAmount v)
        (ingredientBasicUnit v)
        False
        e

makeLenses ''WeighedIngredient

-- | Fetcher to obtain all recipes with their associated weighted ingredients
-- from the database. The query is done in two steps, once for all recipes, then
-- as a loop over those recipes, rather than joining and then collapsing by
-- recipe.
recipes :: Fetcher (Text) (Seq (Entity Recipe))
recipes = dbFetcher $ \str -> do
    let str' = flip T.snoc '%' . T.cons '%' $ str
    rs <- select $ 
          from $ \r -> do
              where_ (r ^. RecipeName `like` val str')
              return r
    pure . S.fromList $ rs

-- | Select all weighted ingredients for some recipe
ingredientsFor :: Fetcher (Key Recipe) [WeighedIngredient]
ingredientsFor = dbFetcher $ \recipe -> do
    xs <- select $
            from $ \(h,i) -> do
                where_ (h ^. RecipeHasRecipe ==. val recipe
                    &&. i ^. IngredientId ==. h ^. RecipeHasIngredient)

                return (h ^. RecipeHasAmount, h ^. RecipeHasUnit
                       ,h ^. RecipeHasOptional, i)

    pure $ map 
        (\(a,b,c,d) -> WeighedIngredient (unValue a) (unValue b) (unValue c) d) 
        xs

-- | Select all ingredient names in the DB
completionList :: Fetcher () [Text]
completionList = dbFetcher $ \_ ->
    map (ingredientName . entityVal) <$> P.selectList [] []

ingredientByName :: Fetcher Text (Entity Ingredient)
ingredientByName = filterMaybe . dbFetcher $ \name ->
    P.selectFirst [ IngredientName P.==. name ] []

updateRecipe :: Consumer (Entity Recipe)
updateRecipe = dbConsumer $ \(Entity k r) -> P.repsert k r

newRecipe :: Fetcher () (Entity Recipe)
newRecipe = dbFetcher $ \_ ->
    P.insertEntity $ 
        Recipe "New Recipe" "Cuisine" 0 "" 0 1 "Serving" Nothing Nothing

deleteRecipe :: Consumer (Key Recipe)
deleteRecipe = dbConsumer $ \k -> P.delete k

newIngredient :: Fetcher Ingredient (Entity Ingredient)
newIngredient = dbFetcher P.insertEntity
