{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Garlic.Presenter.Recipe 
(
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

import qualified Data.Sequence as S

recipeP :: GarlicApp -> Event (Entity Recipe) -> Event Text -> Garlic ()
recipeP app newKey search = mdo
    -- Recipe Search
    rcps   <- do
        let refetch = search <:> ("" <$ app ^. appStartup) <:> ("" <$ newKey)
        refetched <- fmap const <$> fetch recipes refetch
        accumB mempty $ unions [ refetched , fst <$> editChange ]
    
    -- Selection Event holding current recipe entity
    let selected = (S.index <$> rcps) <@> app ^. appVRecipes 
                                        . vrRecipeList . recipeSelected
               <:> newKey
               <:> (snd <$> editChange)

    -- Subsystems
    editChange <- recipeEditP app selected
    recipeDisplayP app selected
    recipeList app rcps

    return ()

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
