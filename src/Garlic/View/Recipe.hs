{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.Recipe 
(
    GarlicViewRecipes (..),
    vrRecipeEdit,
    vrRecipeDisplay,
    vrRecipeList,
    viewRecipes,
    
    -- * Recipe List
    GarlicRecipes,
    addRecipes,
    clearRecipes,
    recipeSelected,

    ListRecipe (..),
    lrRating,
    lrDuration,
    lrName,
    lrCuisine,
) 
where

import Control.Lens.TH
import Control.Monad.Trans
import Data.FileEmbed
import Data.Sequence (Seq)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Garlic.Types
import Reactive.Banana.Frameworks (mapEventIO)
import Reactive.Banana.GI.Gtk
import Text.Printf

import Garlic.View.Recipe.Display
import Garlic.View.Recipe.Edit
import Garlic.Data.Duration

uiViewRecipes :: Text
uiViewRecipes = decodeUtf8 $(embedFile "res/view-recipes.ui")

uiRecipeEntry :: Text
uiRecipeEntry = decodeUtf8 $(embedFile "res/recipe-entry.ui")

data GarlicViewRecipes = GarlicViewRecipes
    { _vrRecipeDisplay :: GarlicRecipeDisplay
    , _vrRecipeEdit    :: GarlicRecipeEdit
    , _vrRecipeList    :: GarlicRecipes
    }

viewRecipes :: (Garlic EntryCompletion) -> Stack -> Garlic GarlicViewRecipes
viewRecipes newCompl stack = do
    b <- builderNew
    _ <- builderAddFromString b uiViewRecipes (-1)

    container <- castB b "paned" Paned
    rlist <- castB b "recipeList" ListBox
    rstack <- castB b "recipeStack" Stack
    rdis <- recipeDisplay rstack
    redt <- recipeEdit rstack newCompl
    recs <- recipes rlist

    stackAddTitled stack container "view-recipes" "Recipes"

    pure $ GarlicViewRecipes rdis redt recs

-- | Used by 'addRecipes' in 'GarlicRecipes' to add new recipes to the list
data ListRecipe = ListRecipe 
    { _lrRating   :: Int
    , _lrDuration :: Int
    , _lrName     :: Text
    , _lrCuisine  :: Text
    }

data GarlicRecipes = GarlicRecipes
    { _clearRecipes   :: Consumer ()
    , _addRecipes     :: Consumer (Seq ListRecipe)
    , _recipeSelected :: Event Int
    }

recipes :: ListBox -> Garlic GarlicRecipes
recipes rlist = 
    lift $ GarlicRecipes
       <$> pure (ioConsumer $ \_ -> clearList rlist)
       <*> pure (ioConsumer $ mapM_ append)
       <*> (mapEventIO (fmap fromIntegral . listBoxRowGetIndex) 
                =<< signalE1 rlist #rowActivated)
    
    where clearList l = 
              containerGetChildren l >>= mapM_ (containerRemove l)
          append (ListRecipe a b d e) = 
              recipeEntry a b d e >>= \x -> listBoxInsert rlist x (-1)
 
-- | Build a new ListBoxRow for a recipe entry in the recipe sidebar/list.
recipeEntry 
    :: MonadIO m 
    => Int          -- ^ Rating
    -> Int          -- ^ Time Required
    -> Text         -- ^ Name
    -> Text         -- ^ Cuisine
    -> m ListBoxRow
recipeEntry rate time name cuisine = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeEntry (-1)
    
    nameL <- castB b "recipeName" Label
    infoL <- castB b "recipeInfo" Label

    let info  = pack $ printf "%s —  %s — %s" rating cuisine time'

    set nameL [ #label := name ]
    set infoL [ #label := info ]

    castB b "recipeEntry" ListBoxRow

    where rating = ratingString rate
          time'  = durationString time 

-- LENSES
makeLenses ''ListRecipe
makeGetters ''GarlicViewRecipes
makeGetters ''GarlicRecipes
