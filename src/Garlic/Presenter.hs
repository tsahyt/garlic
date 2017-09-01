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
import Text.Printf
import Data.Text (pack)

import qualified Data.IntMap as M

import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay

import Garlic.Model
import Garlic.Model.Queries

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    rs <- stepper M.empty =<< fetch recipes (app ^. appStartup)
    selectedIndex <- stepper 0 (app ^. appRecipeList . recipeSelected)

    let rsList  = fmap fst <$> rs
        current = M.lookup <$> selectedIndex <*> rs
        display = app ^. appActivate

    displayRecipe (app ^. appRecipeDisplay) `consumeMaybe` current <@ display
    listRecipes app `consume` rsList <@ display
    stdout `consume` show <$> rs <@ display

    return ()

listRecipes :: GarlicApp -> Consumer (IntMap Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine

displayRecipe :: GarlicRecipeDisplay -> Consumer (Recipe, [WeighedIngredient])
displayRecipe rdisp = mconcat
    [ rdisp ^. clearIngredients $< ()
    , recipeInstructions . fst >$< rdisp ^. loadInstructions
    , map mkig . snd >$< rdisp ^. addIngredients
    ]
    where mkig :: WeighedIngredient -> ViewIngredient
          mkig WeighedIngredient{..} = 
              let m = pack $ printf "%.2f %s" wingrAmount wingrUnit
               in ViewIngredient m (ingredientName wingrIngr)
