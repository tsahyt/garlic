{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Garlic.View
(
    -- * Application Window
    GarlicApp,
    appHeader,
    appRecipeEdit,
    appRecipeDisplay,
    appRecipeList,
    appEnableSearch,
    appDisplayError,
    appSearchChange,
    appActivate,
    appShutdown,
    appStartup,
    application,

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
import Garlic.Types
import Reactive.Banana.Frameworks (mapEventIO)
import Reactive.Banana.GI.Gtk
import Data.FileEmbed
import Data.Text (Text, pack)
import Text.Printf
import Data.Text.Encoding (decodeUtf8)
import Data.Sequence (Seq)
import GI.Gtk

import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay
import Garlic.View.RecipeEdit
import Garlic.Util

uiMainWindow :: Text
uiMainWindow = decodeUtf8 $(embedFile "res/main-window.ui")

uiRecipeEntry :: Text
uiRecipeEntry = decodeUtf8 $(embedFile "res/recipe-entry.ui")

data GarlicApp = GarlicApp
    { _appHeader        :: GarlicHeader
    , _appRecipeDisplay :: GarlicRecipeDisplay
    , _appRecipeEdit    :: GarlicRecipeEdit
    , _appRecipeList    :: GarlicRecipes
    , _appEnableSearch  :: Consumer ()
    , _appDisplayError  :: Consumer Text
    , _appSearchChange  :: Event Text
    , _appActivate      :: Event ()
    , _appStartup       :: Event ()
    , _appShutdown      :: Event ()
    }

application :: Application -> Garlic GarlicApp
application app = do
    b <- builderNew
    _ <- builderAddFromString b uiMainWindow (-1)

    -- Widgets
    win         <- castB b "applicationWindow" ApplicationWindow
    rlist       <- castB b "recipeList" ListBox
    rstack      <- castB b "recipeStack" Stack
    searchBar   <- castB b "searchBar" SearchBar
    searchEntry <- castB b "searchEntry" SearchEntry
    infoBar     <- castB b "infoBar" InfoBar
    infoLabel   <- castB b "infoLabel" Label

    -- Sub elements
    hb   <- headerBar win
    rdis <- recipeDisplay rstack
    redt <- recipeEdit rstack
    recs <- recipes rlist

    -- Hardcoded window setting on activation
    _ <- on app #activate $ do
        set win [ #application := app ]
        widgetShowAll win

    -- Hardcode info hide
    _ <- on infoBar #close $ set infoBar [ #visible := False ]
    _ <- on infoBar #response $ \_ -> set infoBar [ #visible := False ]

    lift $ GarlicApp 
       <$> pure hb                          -- HeaderBar
       <*> pure rdis                        -- Recipe Display
       <*> pure redt                        -- Recipe Editor
       <*> pure recs                        -- Recipe List
       <*> pure (searchToggle searchBar)    -- Search Toggle
       <*> pure (ioConsumer $ \t -> do
               set infoBar [ #visible := True ]
               set infoLabel [ #label := t ])
       <*> (mapEventIO (\_ -> get searchEntry #text) 
                =<< signalE0 searchEntry #searchChanged)
       <*> signalE0 app #activate
       <*> signalE0 app #startup
       <*> signalE0 app #shutdown

-- | Toggle a 'SearchBar'
searchToggle :: SearchBar -> Consumer ()
searchToggle s = ioConsumer $ \_ -> do
    x <- get s #searchModeEnabled
    set s [ #searchModeEnabled := not x ]

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

-- LENSES --
makeLenses ''ListRecipe
makeGetters ''GarlicApp
makeGetters ''GarlicRecipes
