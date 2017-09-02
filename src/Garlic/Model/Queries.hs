{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.Model.Queries
(
    recipes,
    ingredientsFor,
    WeighedIngredient (..),
    wingrAmount,
    wingrUnit,
    wingrIngr,
)
where

import Control.Lens.TH
import Garlic.Model
import Garlic.Types
import Database.Esqueleto
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Text as T

-- | Weighted ingredients are ingredients with an amount and a unit.
data WeighedIngredient = WeighedIngredient
    { _wingrAmount :: Double
    , _wingrUnit   :: Text
    , _wingrIngr   :: Ingredient
    }
    deriving Show

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

                return (h ^. RecipeHasAmount, h ^. RecipeHasUnit, i)

    pure $ map 
        (\(a,b,c) -> WeighedIngredient (unValue a) (unValue b) (entityVal c)) 
        xs
