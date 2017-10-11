{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.Tracking.FoodLog
(
	foodLogP
) 
where

import Control.Lens
import Control.Monad.IO.Class
import Data.Time
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Linear.Vector
import Linear.V2
import Data.List (find)
import Garlic.Model
import Garlic.Data.Meal
import Garlic.Data.Units
import Garlic.Data.Nutrition
import Database.Persist (entityVal)
import Reactive.Banana
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.FoodLog
import Data.Foldable

foodLogP :: GarlicTrackingFoodLog -> Event () -> Garlic ()
foodLogP fl startup = do
    -- recipe change
    rs <- fetch recipes ("" <$ startup)

    -- reload completion on new recipes
    fl ^. flLoadRecipes `consume` (map (recipeName . entityVal) . toList) <$> rs

    -- adding
    let zipName = (\x y -> (y,x)) <$> fl ^. flName
    newEntry <- fetch recipeShort $ zipName <@> fl ^. flAdding
    
    fl ^. flInsert `consume`
        (shortToLog <$> fl ^. flAmount) <@> filterJust newEntry

shortToLog :: Double -> (Meal, Text, Double, [WeighedIngredient]) -> LogRecipe
shortToLog f (m, n, y, ws) =
    LogRecipe
    { lrMeal = m
    , lrName = n
    , lrKcal = nlKcal label
    , lrProtein = nlProtein label ^. _x
    , lrCarbs = nlCarbs label ^. _x
    , lrFat = nlFat label ^. _x
    }
  where
    label = (f / y) *^ foldMap (toLabel defaultReferencePerson) ws
