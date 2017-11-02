{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Garlic.Presenter.Tracking.FoodLog
(
    foodLogP
)
where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Time
import Database.Persist (Entity(..))
import Database.Persist.Sql (SqlPersistT)
import Garlic.Data.Nutrition
import Garlic.Data.Units
import Garlic.Model
import Garlic.Model.EntryTag
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.FoodLog
import Linear.V2
import Linear.Vector
import Reactive.Banana

foodLogP ::
       GarlicTrackingFoodLog
    -> Event ()
    -> Behavior Bool
    -> Consumer [Day]
    -> Behavior Day
    -> Event ()
    -> Garlic (Event ())
foodLogP fl rchange active mark day startup = do
    let time = UTCTime <$> day <*> pure 0
    now <- (\x -> x {utctDayTime = 0}) <$> liftIO getCurrentTime

    -- recipe change
    rs <- dbFetch $ recipes "" <$ unionl [startup, rchange]
    
    -- reload completion on new recipes
    fl ^. flLoadRecipes `consume` (map (recipeName . entityVal) . toList) <$> rs

    -- adding
    foodEntry <- filterJust <$$> dbFetch $ (adding <$> time) <@> fl ^. flAdding

    -- reload on day change or on recipe database change
    dayChange <- plainChanges time
    reload <-
        dbFetch $
        reloadEntries <$> unionl [dayChange, now <$ startup, time <@ rchange]
    fl ^. flClean `consume` () <$ reload

    -- list insertion
    reloadIns <- spread reload
    fl ^. flInsert `consume` unionl [reloadIns, foodEntry]

    -- db deletion
    deleted <- dbFetch $ deleteFoodEntry . lrKey <$> fl ^. flDelete

    -- emit changed event
    let changed = unionl [() <$ reload, () <$ foodEntry, deleted]
    markActive <- plainChanges active

    -- calendar marks
    days <- dbFetch $ getEntryDays <$ unionl [changed, () <$ markActive]
    mark `consume` whenE active (map utctDay <$> days)

    -- amount updating
    changeAmountFoodEntry `consume` fl ^. flAmountEdit
    pure changed

adding :: UTCTime -> Adding -> SqlPersistT IO (Maybe LogRecipe)
adding t (AddingRecipe meal text amount) =
    runMaybeT $ do
        (erecipe, is) <- MaybeT $ recipeShort meal text
        let entry =
                FoodEntry
                    t
                    EntryRecipe
                    (Just . entityKey $ erecipe)
                    Nothing
                    Nothing
                    amount
                    meal
        entry' <- lift $ addFoodEntry entry
        pure $ recipeToLog (entityVal erecipe) is entry'
adding t (AddingIngredient meal text amount unit) =
    runMaybeT $ do
        i <- MaybeT $ ingredientByName text
        let entry =
                FoodEntry
                    t
                    EntryIngredient
                    Nothing
                    (Just . entityKey $ i)
                    (Just unit)
                    amount
                    meal
        entry' <- lift $ addFoodEntry entry
        pure $ ingredientToLog i entry'

reloadEntries :: UTCTime -> SqlPersistT IO [LogRecipe]
reloadEntries t = do
    es <- getFoodEntries t
    forM es $ \e ->
        case foodEntryRef (entityVal e) of
            Left recipe -> do
                is <- ingredientsFor recipe
                r <- getRecipe recipe
                pure $ recipeToLog (entityVal r) is e
            Right ingredient -> do
                i <- getIngredient ingredient
                pure $ ingredientToLog i e

recipeToLog :: Recipe -> [WeighedIngredient] -> Entity FoodEntry -> LogRecipe
recipeToLog r is e =
    LogRecipe
    { lrMeal = foodEntryMeal (entityVal e)
    , lrName = recipeName r
    , lrAmount = foodEntryAmount (entityVal e)
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

ingredientToLog :: Entity Ingredient -> Entity FoodEntry -> LogRecipe
ingredientToLog i e =
    LogRecipe
    { lrMeal = foodEntryMeal (entityVal e)
    , lrName = ingredientName (entityVal i)
    , lrAmount = foodEntryAmount (entityVal e)
    , lrKcal = nlKcal label
    , lrProtein = nlProtein label ^. _x
    , lrCarbs = nlCarbs label ^. _x
    , lrFat = nlFat label ^. _x
    , lrKey = entityKey e
    }
  where
    label =
        toLabel
            defaultReferencePerson
            (WeighedIngredient amount unit False Nothing Nothing i)
    amount = foodEntryAmount . entityVal $ e
    meal = foodEntryMeal . entityVal $ e
    unit = fromMaybe Gram . foodEntryUnit . entityVal $ e
