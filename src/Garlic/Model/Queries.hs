{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
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
    wingrDisp,
    allIngredientNames,
    ingredientByName,

    -- * Updates
    newRecipe,
    updateRecipe,
    deleteRecipe,
    newIngredient,
    deleteIngredient,
    updateIngredient
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
    , _wingrDisp     :: Maybe Text
    , _wingrIngr     :: Entity Ingredient
    }
    deriving Show

fromIngredient :: Entity Ingredient -> WeighedIngredient
fromIngredient e@(Entity _ v) = 
    WeighedIngredient 
        (ingredientBasicAmount v)
        (ingredientBasicUnit v)
        False
        Nothing
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
                       ,h ^. RecipeHasOptional, h ^. RecipeHasDisplay, i)

    pure $ map 
        (\(a,b,c,d,e) -> 
            WeighedIngredient (unValue a) (unValue b) (unValue c) (unValue d) e
        ) 
        xs

-- | Select all ingredient names in the DB
allIngredientNames :: Fetcher () [Text]
allIngredientNames = dbFetcher $ \_ ->
    map (ingredientName . entityVal) <$> P.selectList [] []

ingredientByName :: Fetcher Text (Entity Ingredient)
ingredientByName = filterMaybe . dbFetcher $ \name ->
    P.selectFirst [ IngredientName P.==. name ] []

updateRecipe :: Consumer (Entity Recipe, [WeighedIngredient])
updateRecipe = dbConsumer $ \(Entity k r, is) -> do
    -- update the recipe entry
    P.repsert k r

    -- remove all old associations to ingredients
    delete $
        from $ \h ->
        where_ (h ^. RecipeHasRecipe ==. val k)

    -- insert new ones
    P.insertMany_ $
        [ RecipeHas k (entityKey _wingrIngr) _wingrAmount 
                    _wingrUnit _wingrOptional _wingrDisp
        | WeighedIngredient{..} <- is ]

newRecipe :: Fetcher () (Entity Recipe)
newRecipe = dbFetcher $ \_ ->
    P.insertEntity $ 
        Recipe "New Recipe" "Cuisine" 0 "" 0 1 "Serving" Nothing Nothing

deleteRecipe :: Consumer (Key Recipe)
deleteRecipe = dbConsumer $ \k -> do
    delete $
        from $ \h ->
            where_ (h ^. RecipeHasRecipe ==. val k)
    P.delete k

newIngredient :: Fetcher Ingredient (Maybe (Entity Ingredient))
newIngredient = dbFetcher $ \i -> do
    x <- P.selectFirst [ IngredientName P.==. ingredientName i ] []
    case x of
        Just _  -> pure Nothing
        Nothing -> Just <$> P.insertEntity i

deleteIngredient :: Consumer (Key Ingredient)
deleteIngredient = dbConsumer $ \k -> do
    delete $
        from $ \h ->
            where_ (h ^. RecipeHasIngredient ==. val k)
    P.delete k

updateIngredient :: Consumer (Key Ingredient, Ingredient)
updateIngredient = dbConsumer $ \(k,i) -> P.repsert k i
