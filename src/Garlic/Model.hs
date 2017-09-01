{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Garlic.Model where

import Data.Text (Text)
import Database.Persist.Sql
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

    Ingredient
        name Text

    RecipeHas
        recipe RecipeId
        ingredient IngredientId
 |]
