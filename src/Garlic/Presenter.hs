{-# LANGUAGE RecursiveDo #-}
module Garlic.Presenter where

import Control.Lens
import Database.Persist.Sql
import GI.Gtk (Application)
import Garlic.Types
import Reactive.Banana

import Garlic.Model
import Garlic.Model.CSV
import Garlic.Model.Queries
import Garlic.Presenter.Recipe
import Garlic.Presenter.Tracking
import Garlic.Presenter.IngredientEditor
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.IngredientEditor (ieRun)

import qualified Data.Text as T

presenter :: Application -> Garlic ()
presenter app' = mdo
    runMigration migrateAll
    app <- application app'

    -- New Recipe
    newKey <- fetch newRecipe $ app ^. appHeader . addClick

    -- Search Event
    search <- searchBar app

    -- Ingredient Completion
    completion <- fetch allIngredientNames $
            app ^. appStartup 
        <:> (() <$ app ^. appAppMenu . amIngImport)
    app ^. appReplaceIngr `consume` completion

    -- AppMenu
    app ^. appIngredientEd . ieRun `consume` app ^. appAppMenu . amIngEditor
    importCSV `consume` app ^. appAppMenu . amIngImport
    app ^. appQuit `consume` app ^. appAppMenu . amQuit
    app ^. appAbout `consume` app ^. appAppMenu . amAbout

    -- Subsystems
    rv <- recipeP app newKey search
    trackingP app
    ingredientEditorP app
    headerButtons (app ^. appHeader) rv

    return ()

headerButtons :: GarlicHeader -> Behavior RecipeView -> Garlic ()
headerButtons hb rv = do
    change <- plainChanges $ (,) <$> hb ^. mainView <*> rv

    let toTracking = filterE ((== MainTracking) . fst) change
        toDisplay  = filterE (== (MainRecipes, RecipeViewDisplay)) change
        toEdit     = filterE (== (MainRecipes, RecipeViewEdit)) change

    -- toEdit
    hb ^. yieldToggle `consume` False <$ toEdit
    hb ^. addToggle `consume` False <$ toEdit
    hb ^. editToggle `consume` False <$ toEdit
    hb ^. backToggle `consume` True <$ toEdit
    hb ^. searchToggle `consume` False <$ toEdit

    -- toDisplay
    hb ^. yieldToggle `consume` True <$ toDisplay
    hb ^. addToggle `consume` True <$ toDisplay
    hb ^. editToggle `consume` True <$ toDisplay
    hb ^. backToggle `consume` False <$ toDisplay
    hb ^. searchToggle `consume` True <$ toDisplay

    -- toTracking
    hb ^. yieldToggle `consume` False <$ toTracking
    hb ^. addToggle `consume` False <$ toTracking
    hb ^. editToggle `consume` False <$ toTracking
    hb ^. backToggle `consume` False <$ toTracking
    hb ^. searchToggle `consume` False <$ toTracking

-- | Search bar handling, returning events declaring the current search string
searchBar :: GarlicApp -> Garlic (Event T.Text)
searchBar app = do
    -- Toggle Search Bar
    let toggle = app ^. appHeader . searchToggled
    app ^. appEnableSearch `consume` toggle
    pure $ app ^. appSearchChange
