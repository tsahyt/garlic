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
    appEnableSearch,
    application
)
where

import Garlic.Types
import Control.Monad
import Control.Monad.Trans
import Reactive.Banana (Event)
import Reactive.Banana.GI.Gtk
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk

import Garlic.View.HeaderBar

uiMainWindow :: Text
uiMainWindow = decodeUtf8 $(embedFile "res/main-window.ui")

data GarlicApp = GarlicApp
    { _appHeader       :: GarlicHeader
    , _appEnableSearch :: Consumer ()
    }

application :: Application -> Garlic GarlicApp
application app = do
    b <- builderNew
    _ <- builderAddFromString b uiMainWindow (-1)
    win <- castB b "applicationWindow" ApplicationWindow

    hb <- headerBar win
    searchBar <- castB b "searchBar" SearchBar

    on app #activate $ do
        set win [ #application := app ]
        widgetShowAll win

    return $ GarlicApp hb (searchToggle searchBar)

searchToggle :: SearchBar -> Consumer ()
searchToggle s = ioConsumer $ \_ -> do
    x <- get s #searchModeEnabled
    set s [ #searchModeEnabled := not x ]

-- LENSES --
makeGetters ''GarlicApp
