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
data WeighedIngredient = WeighedIngredient
    { wingrAmount :: Double
    , wingrUnit   :: Text
    , wingrIngr   :: Ingredient
    }
    deriving Show

recipes :: Fetcher a (IntMap (Recipe, [WeighedIngredient]))
recipes = dbFetcher $ \_ -> do
    rs <- getRecipeData
    xs <- forM rs $ \r -> do
              is <- ingredientsFor (entityKey r)
              pure (entityVal r, is)
    pure . M.fromList . zip [0..] $ xs

getRecipeData :: DB [Entity Recipe]
getRecipeData = select $ from $ \r -> return r

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
