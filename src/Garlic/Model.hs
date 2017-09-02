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
import Database.Persist.TH
import Text.Markdown
import Text.Markdown.Persist ()

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
    Recipe
        name Text
        cuisine Text
        rating Int
        instructions Markdown
        duration Int
        yield Double
        source Text Maybe
        url Text Maybe
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
