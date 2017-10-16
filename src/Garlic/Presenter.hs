{-# LANGUAGE RecursiveDo #-}
module Garlic.Presenter where

import Control.Lens
import Database.Persist.Sql
import GI.Gtk (Application)
import Garlic.Types

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
    recipeP app newKey search
    trackingP app
    ingredientEditorP app

    return ()

-- | Search bar handling, returning events declaring the current search string
searchBar :: GarlicApp -> Garlic (Event T.Text)
searchBar app = do
    -- Toggle Search Bar
    let toggle = app ^. appHeader . searchToggled
    app ^. appEnableSearch `consume` toggle
    pure $ app ^. appSearchChange
