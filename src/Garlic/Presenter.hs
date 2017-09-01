{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter where

import Control.Lens
import Data.Functor.Contravariant
import Database.Persist.Sql
import Reactive.Banana
import GI.Gtk (Application)
import Garlic.Types

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Presenter.RecipeDisplay
import Garlic.View
import Garlic.View.HeaderBar

import qualified Data.Text as T
import qualified Data.IntMap as M

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    let refetch = app ^. appStartup
    rcps <- stepper mempty =<< fetch recipes refetch

    -- Subsystems
    (rcps', searching) <- search app rcps
    recipeDisplayP app rcps'

    -- Recipe List (TODO!)
    let update = app ^. appActivate 
             <:> whenE searching (app ^. appSearchChange)
    listRecipes app `consume` fmap fst <$> rcps' <@ update

    return ()

-- | Description of behaviour for the search system.
search
    :: GarlicApp
    -> Behavior (M.IntMap (Recipe, a))
    -> Garlic (Behavior (M.IntMap (Recipe, a)), Behavior Bool)
search app rcps = do
    -- Toggle Search Bar
    let toggle = app ^. appHeader . searchToggled
    app ^. appEnableSearch `consume` toggle

    let searchString = app ^. appSearchString
        rcps' = filterRecipes <$> searchString <*> rcps
    active <- accumB False (not <$ toggle)

    -- TODO: Does not always seem to work as intended. Also selection of search
    -- results seems off

    pure (rcps', active)

    where filterRecipes str
              | T.null str = id
              | otherwise  = M.filter (T.isInfixOf str . recipeName . fst)

-- | Consumer to populate the recipe list.
listRecipes :: GarlicApp -> Consumer (M.IntMap Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine
