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

    -- Toggle Search Bar
    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    let refetch = app ^. appStartup
    rcps <- stepper mempty =<< fetch recipes refetch

    -- Subsystems
    recipeDisplayP app rcps

    -- Recipe List (TODO!)
    listRecipes app `consume` fmap fst <$> rcps <@ app ^. appActivate

    return ()

-- | Convenience function to specify search semantics on the underlying IntMap
-- type.
filterRecipes :: T.Text -> M.IntMap (Recipe, a) -> M.IntMap (Recipe, a)
filterRecipes search
    | T.null search = id
    | otherwise     = M.filter (\(r,_) -> T.isInfixOf search (recipeName r))

-- | Consumer to populate the recipe list.
listRecipes :: GarlicApp -> Consumer (M.IntMap Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine
