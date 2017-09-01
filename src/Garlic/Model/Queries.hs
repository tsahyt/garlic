{-# LANGUAGE OverloadedStrings #-}
module Garlic.Model.Queries
(
    WeighedIngredient (..),
    recipes
)
where

import Control.Monad
import Garlic.Model
import Garlic.Types
import Database.Esqueleto
import Data.Text (Text)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

type DB a = SqlPersistT IO a

-- | Weighted ingredients are ingredients with an amount and a unit.
data WeighedIngredient = WeighedIngredient
    { wingrAmount :: Double
    , wingrUnit   :: Text
    , wingrIngr   :: Ingredient
    }
    deriving Show

-- | Fetcher to obtain all recipes with their associated weighted ingredients
-- from the database. The query is done in two steps, once for all recipes, then
-- as a loop over those recipes, rather than joining and then collapsing by
-- recipe.
recipes :: Fetcher a (IntMap (Recipe, [WeighedIngredient]))
recipes = dbFetcher $ \_ -> do
    rs <- select $ from $ \r -> return r
    xs <- forM rs $ \r -> do
              is <- ingredientsFor (entityKey r)
              pure (entityVal r, is)
    pure . M.fromList . zip [0..] $ xs

-- | Select all weighted ingredients for some recipe
ingredientsFor :: Key Recipe -> DB [WeighedIngredient]
ingredientsFor recipe = do
    xs <- select $
            from $ \(h,i) -> do
                where_ (h ^. RecipeHasRecipe ==. val recipe
                    &&. i ^. IngredientId ==. h ^. RecipeHasIngredient)

                return (h ^. RecipeHasAmount, h ^. RecipeHasUnit, i)

    pure $ map 
        (\(a,b,c) -> WeighedIngredient (unValue a) (unValue b) (entityVal c)) 
        xs
