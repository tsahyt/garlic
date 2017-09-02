{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter where

import Control.Lens
import Data.Functor.Contravariant
import Data.Sequence (Seq)
import Database.Persist.Sql
import GI.Gtk (Application)
import Garlic.Types
import Reactive.Banana

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Presenter.RecipeDisplay
import Garlic.View
import Garlic.View.HeaderBar

import qualified Data.Text as T
import qualified Data.Sequence as S

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
    listRecipes app `consume` fmap entityVal <$> rcps' <@ update

    return ()

-- | Description of behaviour for the search system.
search
    :: GarlicApp
    -> Behavior (Seq (Entity Recipe))
    -> Garlic (Behavior (Seq (Entity Recipe)), Behavior Bool)
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
              | otherwise  = S.filter (T.isInfixOf str . recipeName . entityVal)

-- | Consumer to populate the recipe list.
listRecipes :: GarlicApp -> Consumer (Seq Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine
