{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.RecipeDisplay
(
    recipeDisplayP
)
where

import Control.Lens
import Reactive.Banana
import Data.Functor.Contravariant
import Data.IntMap (IntMap)
import Data.Text (Text, pack)
import Text.Printf

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View
import Garlic.View.RecipeDisplay

import qualified Data.IntMap as M
import qualified Data.Text as T

recipeDisplayP 
    :: GarlicApp 
    -> Behavior (IntMap (Recipe, [WeighedIngredient])) 
    -> Garlic ()
recipeDisplayP app rcps = do
    -- Selection Event holding current recipe
    let selected = filterJust 
              $ (flip M.lookup <$> rcps) 
            <@> app ^. appRecipeList . recipeSelected

    displayRecipe (app ^. appRecipeDisplay) `consume` selected

-- | Consumer to display a selected recipe.
displayRecipe :: GarlicRecipeDisplay -> Consumer (Recipe, [WeighedIngredient])
displayRecipe rdisp = mconcat
    [ rdisp ^. clearIngredients $< ()
    , recipeInstructions . fst >$< rdisp ^. loadInstructions
    , map mkig . snd >$< rdisp ^. addIngredients
    ]
    where mkig :: WeighedIngredient -> ViewIngredient
          mkig WeighedIngredient{..} = 
              let m = pack $ printf "%G %s" wingrAmount wingrUnit
               in ViewIngredient m (ingredientName wingrIngr)
