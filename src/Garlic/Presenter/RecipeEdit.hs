{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.RecipeEdit
(
    recipeEditP
)
where

import Control.Lens
import Data.Maybe
import Data.Functor.Compose
import Data.Text (pack)
import Database.Persist.Sql
import Reactive.Banana

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeEdit
import Garlic.View.RecipeDisplay
import Text.Read

import qualified Data.Text as T

recipeEditP
    :: GarlicApp
    -> Event (Entity Recipe)
    -> Garlic ()
recipeEditP app selected = do
    key <- stepper Nothing (Just . entityKey <$> selected)

    -- Show Editor on Edit Click, hide Edit Button and yield
    let click = app ^. appHeader . editClick
     in do app ^. appRecipeEdit . showEditor `consume` click
           app ^. appHeader . yieldToggle `consume` click
           app ^. appHeader . editToggle `consume` click

    loadRecipe app (entityVal <$> selected)
    recipe <- currentRecipe app

    -- Recipe Entity, only Just when there is also a previous selection
    let recipeEntity = getCompose $ Entity
                   <$> Compose key 
                   <*> Compose (Just <$> recipe)
    
    -- Save on Store Click, or do nothing when there was no selection
    updateRecipe `consume` 
        filterJust (recipeEntity <@ app ^. appRecipeEdit . editStore)

    -- Show Display on Abort Click
    let click = app ^. appRecipeEdit . editAbort
     in do app ^. appRecipeDisplay . showDisplay `consume` click
           app ^. appHeader . yieldToggle `consume` click
           app ^. appHeader . editToggle `consume` click

-- | Load recipe into mask on selection event
loadRecipe :: GarlicApp -> Event Recipe -> Garlic ()
loadRecipe app rcp = do
    let masks = app ^. appRecipeEdit . editMasks
    masks ^. editSetName `consume` recipeName <$> rcp
    masks ^. editSetCuisine `consume` recipeCuisine <$> rcp
    masks ^. editSetDuration `consume` pack . show . recipeDuration <$> rcp
    masks ^. editSetYield `consume` recipeYield <$> rcp
    masks ^. editSetYieldUnit `consume` recipeYieldUnit <$> rcp
    masks ^. editSetSource `consume` fromMaybe "" . recipeSource <$> rcp
    masks ^. editSetURL `consume` fromMaybe "" . recipeUrl <$> rcp
    masks ^. editSetRating `consume` recipeRating <$> rcp

    app ^. appRecipeEdit . editSetInstructions 
        `consume` recipeInstructions <$> rcp

currentRecipe :: GarlicApp -> Garlic (Behavior Recipe)
currentRecipe app = do
    let masks = app ^. appRecipeEdit . editMasks
        rec   = Recipe 
            <$> masks ^. editName
            <*> masks ^. editCuisine
            <*> masks ^. editRating
            <*> fmap (fromMaybe "") (app ^. appRecipeEdit . editInstructions)
            <*> fmap parseInt (masks ^. editDuration)
            <*> masks ^. editYield
            <*> masks ^. editYieldUnit
            <*> fmap mtext (masks ^. editSource)
            <*> fmap mtext (masks ^. editURL)
    pure rec

    where mtext x  = if T.null x then Nothing else Just x
          parseInt = fromMaybe 0 . readMaybe . T.unpack
