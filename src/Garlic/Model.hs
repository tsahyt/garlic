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
import Garlic.Model.Units

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
        deriving Show

    RecipeHas
        recipe RecipeId
        ingredient IngredientId
        amount Double
        unit Unit
        deriving Show
 |]
