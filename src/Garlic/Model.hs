{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic.Model where

import Data.Text (Text)
import Data.Maybe (fromMaybe)
import Database.Persist.TH
import Text.Markdown
import Text.Markdown.Persist ()
import Garlic.Data.Units
import Garlic.Data.Meal
import Garlic.Model.EntryTag
import Data.Time.Clock

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Recipe
        name Text
        cuisine Text
        rating Int
        instructions Markdown
        duration Int
        yield Double
        yieldUnit Text
        source Text Maybe
        url Text Maybe
        deriving Show

    Ingredient
        name Text
        comment Text
        basicUnit Unit
        basicAmount Double
        protein Double
        carbs Double
        sugar Double Maybe
        fibre Double Maybe
        fat Double
        satFat Double Maybe
        polyFat Double Maybe
        monoFat Double Maybe
        transFat Double Maybe
        sodium Double Maybe
        cholesterol Double Maybe
        UniqueName name
        deriving Show

    RecipeHas
        recipe RecipeId
        ingredient IngredientId
        amount Double
        unit Unit
        optional Bool
        display Text Maybe
        group Text Maybe
        deriving Show

    Goal
        timestamp UTCTime
        kcal Double
        protein Double
        carbs Double
        sugar Double
        fat Double
        satFat Double
        monoFat Double
        polyFat Double
        sodium Double
        cholesterol Double
        weight Double
        unit Unit
        deriving Show

    WeightMeasurement
        timestamp UTCTime
        weight Double
        unit Unit
        deriving Show

    FoodEntry
        timestamp UTCTime
        tag EntryTag
        recipe RecipeId Maybe
        ingredient IngredientId Maybe
        unit Unit Maybe
        amount Double
        meal Meal
 |]

foodEntryRef :: FoodEntry -> Either RecipeId IngredientId
foodEntryRef e = case foodEntryTag e of
    EntryRecipe -> Left $ fromMaybe err (foodEntryRecipe e)
    EntryIngredient -> Right $ fromMaybe err (foodEntryIngredient e)
    where err = error "Database consistency error!"
