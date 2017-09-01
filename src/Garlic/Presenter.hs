{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter where

import Control.Lens
import Data.IntMap (IntMap)
import Garlic.Types
import Reactive.Banana
import Data.Functor.Contravariant
import GI.Gtk (Application)
import Database.Persist.Sql

import qualified Data.IntMap as M

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
        rs = M.fromList (zip [0..] $ replicate 4 r)
    displayRecipe (app ^. appRecipeDisplay) `consume` 
        (r, is) <$ app ^. appActivate
    listRecipes app `consume` rs <$ app ^. appActivate

    return ()

listRecipes :: GarlicApp -> Consumer (IntMap Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine

displayRecipe :: GarlicRecipeDisplay -> Consumer (Recipe, [ViewIngredient])
displayRecipe rdisp = mconcat
    [ rdisp ^. clearIngredients $< ()
    , recipeInstructions . fst >$< rdisp ^. loadInstructions
    , snd >$< rdisp ^. addIngredients
    ]
