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
    appEnableSearch,
    application
)
where

import Control.Monad.IO.Class
import Garlic.Types
import Reactive.Banana.GI.Gtk
import Data.FileEmbed
import Data.Text (Text, pack)
import Text.Printf
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock
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
    , _appEnableSearch  :: Consumer ()
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

    _ <- on app #activate $ do
        set win [ #application := app ]
        widgetShowAll win

    -- DUMMY VALUES
    let attach x = listBoxInsert rlist x (-1)
     in mapM_ (attach =<<)
            [ recipeEntry 3 (secondsToDiffTime 6000) 120
                  "Excellent Chicken" "Asian"
            , recipeEntry 2 (secondsToDiffTime 1500) 50
                  "Not-so-excellent Chicken" "Jamaican"
            , recipeEntry 4 (secondsToDiffTime 15000) 250
                  "Long Chicken" "Jamaican"
            ]
    -- /DUMMY VALUES

    return $ GarlicApp hb rdis (searchToggle searchBar)

searchToggle :: SearchBar -> Consumer ()
searchToggle s = ioConsumer $ \_ -> do
    x <- get s #searchModeEnabled
    set s [ #searchModeEnabled := not x ]

recipeEntry 
    :: MonadIO m 
    => Int          -- ^ Rating
    -> DiffTime     -- ^ Time Required
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
                      x = fromIntegral (diffTimeToPicoseconds time) / (6.0e13)
                   in if x >= 120
                      then printf "%.1f h" (x / 60)
                      else printf "%d min" (truncate x :: Int)

-- LENSES --
makeGetters ''GarlicApp
