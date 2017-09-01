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
import Garlic.View.HeaderBar

import qualified Data.IntMap as M
import qualified Data.Text as T

recipeDisplayP 
    :: GarlicApp 
    -> Behavior (IntMap (Recipe, [WeighedIngredient])) 
    -> Garlic ()
recipeDisplayP app rcps = do
    let disp = app ^. appRecipeDisplay

    yield <- stepper 1 $ app ^. appHeader . yieldChanged

    -- Selection Event holding current recipe
    let selected = filterJust 
              $ (flip M.lookup <$> rcps) 
            <@> app ^. appRecipeList . recipeSelected
        ryield       = recipeYield . fst <$> selected

    ryield' <- stepper 1 ryield

    let factor       = liftA2 (/) yield ryield'
        ingredients  = (scaleIngredients <$> factor) <@> (snd <$> selected)
        instructions = recipeInstructions . fst <$> selected

    disp ^. loadInstructions `consume` instructions
    replaceIngredients disp `consume` ingredients

    -- First yield to spinner
    app ^. appHeader . changeYield `consume` ryield

replaceIngredients :: GarlicRecipeDisplay -> Consumer [WeighedIngredient]
replaceIngredients disp = mconcat
    [ disp ^. clearIngredients $< ()
    , map mkig >$< disp ^. addIngredients ]
    where mkig :: WeighedIngredient -> ViewIngredient
          mkig WeighedIngredient{..} = 
              let m = pack $ printf "%G %s" _wingrAmount _wingrUnit
               in ViewIngredient m (ingredientName _wingrIngr)

scaleIngredients :: Double -> [WeighedIngredient] -> [WeighedIngredient]
scaleIngredients factor = over (traverse . wingrAmount) (* factor)
