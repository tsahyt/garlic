{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.Tracking.FoodLog
(
    foodLogP
)
where

import Control.Lens
import Data.Time
import Linear.Vector
import Linear.V2
import Garlic.Model
import Garlic.Data.Meal
import Garlic.Data.Nutrition
import Database.Persist (Entity(..))
import Reactive.Banana
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.FoodLog
import Data.Foldable

foodLogP ::
       GarlicTrackingFoodLog -> Behavior Day -> Event () -> Garlic (Event ())
foodLogP fl day startup = do
    let time = UTCTime <$> day <*> pure 0

    -- recipe change
    rs <- fetch recipes ("" <$ startup)

    -- reload completion on new recipes
    fl ^. flLoadRecipes `consume` (map (recipeName . entityVal) . toList) <$> rs

    -- adding
    let zipName = (\x y -> (y, x)) <$> fl ^. flName
    newEntry <- filterJust <$$> fetch recipeShort $ zipName <@> fl ^. flAdding
    foodEntry <-
        fetch addFoodEntry $
        (shortToEntry <$> time <*> fl ^. flAmount) <@> newEntry

    -- reload on day change
    reload <- fetch getFoodEntries =<< plainChanges time
    fl ^. flClean `consume` () <$ reload

    -- list insertion
    reloadIns <- spread reload
    fl ^. flInsert `consume` (\(e, r, is) -> entryToLog r is e) <$>
        unionl [reloadIns, foodEntry]

    -- db deletion
    deleted <- fetch deleteFoodEntry $ lrKey <$> fl ^. flDelete

    -- emit changed event
    pure $ unionl [ () <$ reload, () <$ foodEntry, deleted ]

shortToEntry :: UTCTime -> Double -> (Meal, Entity Recipe, a) -> FoodEntry
shortToEntry t amount (m, r, _) = FoodEntry t (entityKey r) amount m

entryToLog :: Recipe -> [WeighedIngredient] -> Entity FoodEntry -> LogRecipe
entryToLog r is e =
    LogRecipe
    { lrMeal = foodEntryMeal (entityVal e)
    , lrName = recipeName r
    , lrKcal = nlKcal label
    , lrProtein = nlProtein label ^. _x
    , lrCarbs = nlCarbs label ^. _x
    , lrFat = nlFat label ^. _x
    , lrKey = entityKey e
    }
  where
    label =
        (foodEntryAmount (entityVal e) / recipeYield r) *^
        foldMap (toLabel defaultReferencePerson) is
