{-# LANGUAGE TemplateHaskell #-}
module Garlic.Data.Meal (
    Meal (..),
    allMeals
) 
where

import Database.Persist.TH

data Meal
    = Breakfast
    | Lunch
    | Dinner
    | Snack
    deriving (Show, Eq, Read, Ord, Enum, Bounded)

derivePersistField "Meal"

allMeals :: [Meal]
allMeals = enumFrom Breakfast
