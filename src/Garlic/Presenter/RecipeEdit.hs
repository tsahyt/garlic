{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.RecipeEdit
(
    recipeEditP
)
where

import Control.Lens
import Data.Maybe
import Data.Text (pack)
import Database.Persist.Sql
import Reactive.Banana

import Garlic.Model
import Garlic.Types
import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeEdit

recipeEditP
    :: GarlicApp
    -> Event (Entity Recipe)
    -> Garlic ()
recipeEditP app selected = do
    -- Show Editor on Edit Click
    app ^. appRecipeEdit . showEditor `consume` app ^. appHeader . editClick

    loadRecipe app (entityVal <$> selected)

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

    app ^. appRecipeEdit . editSetInstructions 
        `consume` recipeInstructions <$> rcp
