{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter where

import Control.Lens
import Data.Functor.Contravariant
import Data.IntMap (IntMap)
import Data.Text (Text, pack)
import Database.Persist.Sql
import GI.Gtk (Application)
import Garlic.Types
import Text.Printf

import qualified Data.IntMap as M
import qualified Data.Text as T

import Garlic.View
import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay

import Garlic.Model
import Garlic.Model.Queries

presenter :: Application -> Garlic ()
presenter app' = do
    runMigration migrateAll
    app <- application app'

    -- Toggle Search Bar
    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    return ()

-- | Convenience function to specify search semantics on the underlying IntMap
-- type.
filterRecipes :: Text -> IntMap (Recipe, a) -> IntMap (Recipe, a)
filterRecipes search
    | T.null search = id
    | otherwise     = M.filter (\(r,_) -> T.isInfixOf search (recipeName r))

-- | Consumer to populate the recipe list.
listRecipes :: GarlicApp -> Consumer (IntMap Recipe)
listRecipes app = mconcat
    [ app ^. appRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration 0 recipeName recipeCuisine

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
