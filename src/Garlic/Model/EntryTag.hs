{-# LANGUAGE TemplateHaskell #-}
module Garlic.Model.EntryTag
    ( EntryTag(..)
    ) where

import Database.Persist.TH

data EntryTag
    = EntryRecipe
    | EntryIngredient
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

derivePersistField "EntryTag"
