{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter where

import Control.Lens
import Garlic.Types
import Reactive.Banana
import Data.Functor.Contravariant
import GI.Gtk (Application)
import Database.Persist.Sql

import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay

import Garlic.Model

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    let r  = Recipe "Foo" "Bar" 3 "This is a test" 50
        is = [ViewIngredient "2 cups" "stuff"]
    displayRecipe (app ^. appRecipeDisplay) `consume` 
        (r, is) <$ app ^. appActivate

    return ()

displayRecipe :: GarlicRecipeDisplay -> Consumer (Recipe, [ViewIngredient])
displayRecipe rdisp = mconcat
    [ rdisp ^. clearIngredients $< ()
    , recipeInstructions . fst >$< rdisp ^. loadInstructions
    , snd >$< rdisp ^. addIngredients
    ]
