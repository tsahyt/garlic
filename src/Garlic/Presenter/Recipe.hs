{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Garlic.Presenter.Recipe 
(
    RecipeView(..),
    recipeP,
) 
where

import Control.Lens
import Data.Functor.Contravariant
import Data.Sequence (Seq)
import Data.Text (Text)
import Database.Persist.Sql
import Garlic.Types
import Reactive.Banana

import Garlic.Model
import Garlic.Model.Queries
import Garlic.Presenter.Recipe.Display
import Garlic.Presenter.Recipe.Edit
import Garlic.View
import Garlic.View.Recipe
import Garlic.View.Recipe.Edit
import Garlic.View.HeaderBar

import qualified Data.Sequence as S

data RecipeView
    = RecipeViewDisplay
    | RecipeViewEdit
    deriving (Show, Eq, Ord, Enum)

otherView :: RecipeView -> RecipeView
otherView RecipeViewDisplay = RecipeViewEdit
otherView RecipeViewEdit = RecipeViewDisplay

recipeP ::
       GarlicApp
    -> Behavior Goal
    -> Event (Entity Recipe)
    -> Event Text
    -> Garlic (Event (), Behavior RecipeView)
recipeP app goal newKey search = mdo
    -- Recipe Search
    rcps   <- do
        let refetch = search <:> ("" <$ app ^. appStartup) <:> ("" <$ newKey)
        refetched <- fmap const <$> dbFetch (recipes <$> refetch)
        accumB mempty $ unions [ refetched , fst <$> editChange ]
    
    -- Selection Event holding current recipe entity
    let selected =
            unionl
                [ (S.index <$> rcps) <@> app ^. appVRecipes . vrRecipeList .
                  recipeSelected
                , newKey
                , snd <$> editChange
                ]

    -- Subsystems
    editChange <- recipeEditP app selected
    recipeDisplayP app goal selected
    recipeList app rcps

    (,) <$> pure (() <$ editChange) <*> currentView app

currentView :: GarlicApp -> Garlic (Behavior RecipeView)
currentView app = do
    let change =
            unions
                [ const RecipeViewEdit <$ app ^. appHeader . editClick
                , const RecipeViewEdit <$ app ^. appHeader . addClick
                , otherView <$ app ^. appHeader . backClick
                , const RecipeViewDisplay <$ app ^. appVRecipes . vrRecipeEdit .
                  editDelete
                ]
    accumB RecipeViewDisplay change

recipeList :: GarlicApp -> Behavior (Seq (Entity Recipe)) -> Garlic ()
recipeList app rcps = do
    update' <- plainChanges rcps
    listRecipes app `consume` fmap entityVal <$> update'

-- | Consumer to populate the recipe list.
listRecipes :: GarlicApp -> Consumer (Seq Recipe)
listRecipes app = mconcat
    [ app ^. appVRecipes . vrRecipeList ^. clearRecipes $< ()
    , fmap mklr >$< app ^. appVRecipes . vrRecipeList ^. addRecipes ]
    where mklr :: Recipe -> ListRecipe
          mklr Recipe{..} = 
              ListRecipe recipeRating recipeDuration recipeName recipeCuisine
