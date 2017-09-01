{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Garlic.Model where

import Control.Monad
import Data.IntMap (IntMap)
import Data.Text (Text)
import Database.Persist.TH
import Garlic.Types
import Text.Markdown
import Text.Markdown.Persist ()

import qualified Data.IntMap as M

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Recipe
        name Text
        cuisine Text
        rating Int
        instructions Markdown
        duration Int
        deriving Show

    Ingredient
        name Text
        deriving Show

    RecipeHas
        recipe RecipeId
        ingredient IngredientId
        amount Double
        unit Text
        deriving Show
 |]
