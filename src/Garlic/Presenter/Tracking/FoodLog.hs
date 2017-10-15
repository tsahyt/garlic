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
import Database.Persist (Entity(..))
import Reactive.Banana
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.FoodLog
import Data.Foldable

foodLogP :: GarlicTrackingFoodLog -> Behavior Day -> Event () -> Garlic ()
foodLogP fl day startup = do
    let time = UTCTime <$> day <*> pure 0

    -- recipe change
    rs <- fetch recipes ("" <$ startup)

    -- reload completion on new recipes
    fl ^. flLoadRecipes `consume` (map (recipeName . entityVal) . toList) <$> rs

    -- adding
    let zipName = (\x y -> (y,x)) <$> fl ^. flName
    newEntry <- filterJust <$$> fetch recipeShort $ zipName <@> fl ^. flAdding

    let logRecipe = (shortToLog <$> fl ^. flAmount) <@> 
                        (over _2 entityVal <$> newEntry)
        foodEntry = (shortToEntry <$> time <*> fl ^. flAmount) <@> newEntry
    
    fl ^. flInsert `consume` logRecipe
    addFoodEntry `consume` foodEntry

    -- deletion
    stdout `consume` show <$> fl ^. flDelete

    -- reload on day change
    reload <- fetch getFoodEntries =<< plainChanges time
    fl ^. flClean `consume` () <$ reload
    consume (fl ^. flInsert) =<< (\(e,r,is) -> entryToLog e r is) <$$> 
        spread reload

shortToLog :: Double -> (Meal, Recipe, [WeighedIngredient]) -> LogRecipe
shortToLog f (m, r, ws) =
    LogRecipe
    { lrMeal = m
    , lrName = recipeName r
    , lrKcal = nlKcal label
    , lrProtein = nlProtein label ^. _x
    , lrCarbs = nlCarbs label ^. _x
    , lrFat = nlFat label ^. _x
    }
  where
    label =
        (f / recipeYield r) *^
        foldMap (toLabel defaultReferencePerson) ws

shortToEntry :: UTCTime -> Double -> (Meal, Entity Recipe, a) -> FoodEntry
shortToEntry t amount (m, r, _) = FoodEntry t (entityKey r) amount m

entryToLog :: FoodEntry -> Recipe -> [WeighedIngredient] -> LogRecipe
entryToLog e r is = shortToLog (foodEntryAmount e) (foodEntryMeal e, r, is) 
