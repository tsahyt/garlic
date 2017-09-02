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
import Garlic.Presenter.RecipeEdit
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeEdit

import qualified Data.Text as T
import qualified Data.Sequence as S

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    -- Search
    search <- searchBar app
    rcps   <- 
        let refetch = unionWith (\_ _ -> "") search ("" <$ app ^. appStartup)
         in stepper mempty =<< fetch recipes refetch
    
    -- Selection Event holding current recipe entity
    let selected = (S.index <$> rcps)
               <@> app ^. appRecipeList . recipeSelected

    -- Subsystems
    recipeDisplayP app selected
    recipeEditP app selected
    recipeList app rcps

    return ()

-- | Search bar handling, returning events declaring the current search string
searchBar :: GarlicApp -> Garlic (Event T.Text)
searchBar app = do
    -- Toggle Search Bar
    let toggle = app ^. appHeader . searchToggled
    app ^. appEnableSearch `consume` toggle
    pure $ app ^. appSearchChange

recipeList :: GarlicApp -> Behavior (Seq (Entity Recipe)) -> Garlic ()
recipeList app rcps = do
    update' <- plainChanges rcps
    listRecipes app `consume` fmap entityVal <$> update'

-- | Consumer to populate the recipe list.
listRecipes :: GarlicApp -> Consumer (Seq Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine
