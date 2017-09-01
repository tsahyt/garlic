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
    appRecipeDisplay,
    appRecipeList,
    appEnableSearch,
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
    lrKcal,
    lrName,
    lrCuisine,
)
where

import Control.Lens.TH
import Control.Monad.Trans
import Control.Monad.IO.Class
import Garlic.Types
import Reactive.Banana.Frameworks (mapEventIO)
import Reactive.Banana.GI.Gtk
import Data.FileEmbed
import Data.Text (Text, pack)
import Text.Printf
import Data.Text.Encoding (decodeUtf8)
import Data.IntMap (IntMap)
import GI.Gtk

import Garlic.View.HeaderBar
import Garlic.View.RecipeDisplay

uiMainWindow :: Text
uiMainWindow = decodeUtf8 $(embedFile "res/main-window.ui")

uiRecipeEntry :: Text
uiRecipeEntry = decodeUtf8 $(embedFile "res/recipe-entry.ui")

data GarlicApp = GarlicApp
    { _appHeader        :: GarlicHeader
    , _appRecipeDisplay :: GarlicRecipeDisplay
    , _appRecipeList    :: GarlicRecipes
    , _appEnableSearch  :: Consumer ()
    , _appActivate      :: Event ()
    , _appStartup       :: Event ()
    , _appShutdown      :: Event ()
    }

application :: Application -> Garlic GarlicApp
application app = do
    b <- builderNew
    _ <- builderAddFromString b uiMainWindow (-1)
    win <- castB b "applicationWindow" ApplicationWindow

    hb <- headerBar win
    searchBar <- castB b "searchBar" SearchBar
    rstack <- castB b "recipeStack" Stack
    rlist <- castB b "recipeList" ListBox
    rdis <- recipeDisplay rstack
    recs <- recipes rlist

    _ <- on app #activate $ do
        set win [ #application := app ]
        widgetShowAll win

    lift $ GarlicApp 
       <$> pure hb 
       <*> pure rdis 
       <*> pure recs 
       <*> pure (searchToggle searchBar)
       <*> signalE0 app #activate
       <*> signalE0 app #startup
       <*> signalE0 app #shutdown

searchToggle :: SearchBar -> Consumer ()
searchToggle s = ioConsumer $ \_ -> do
    x <- get s #searchModeEnabled
    set s [ #searchModeEnabled := not x ]

data ListRecipe = ListRecipe 
    { _lrRating   :: Int
    , _lrDuration :: Int
    , _lrKcal     :: Int
    , _lrName     :: Text
    , _lrCuisine  :: Text
    }

data GarlicRecipes = GarlicRecipes
    { _clearRecipes   :: Consumer ()
    , _addRecipes     :: Consumer (IntMap ListRecipe)
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
          append (ListRecipe a b c d e) = 
              recipeEntry a b c d e >>= \x -> listBoxInsert rlist x (-1)
recipeEntry 
    :: MonadIO m 
    => Int          -- ^ Rating
    -> Int          -- ^ Time Required
    -> Int          -- ^ Kcal
    -> Text         -- ^ Name
    -> Text         -- ^ Cuisine
    -> m ListBoxRow
recipeEntry rate time kcal name cuisine = do
    b <- builderNew
    _ <- builderAddFromString b uiRecipeEntry (-1)
    
    nameL <- castB b "recipeName" Label
    infoL <- castB b "recipeInfo" Label
    kcalL <- castB b "recipeKcal" Label

    let kcal' = pack $ printf "%dkcal" kcal
        info  = pack $ printf "%s —  %s — %s" rating cuisine time'

    set nameL [ #label := name ]
    set kcalL [ #label := kcal' ]
    set infoL [ #label := info ]

    castB b "recipeEntry" ListBoxRow

    where rating = let x = min 5 (max 0 rate)
                    in replicate x '★' ++ replicate (5 - x) '☆'
          time' :: String
          time' = let x :: Double
                      x = fromIntegral time / (6.0e13)
                   in if x >= 120
                      then printf "%.1f h" (x / 60)
                      else printf "%d min" (truncate x :: Int)

-- LENSES --
makeLenses ''ListRecipe
makeGetters ''GarlicApp
makeGetters ''GarlicRecipes
