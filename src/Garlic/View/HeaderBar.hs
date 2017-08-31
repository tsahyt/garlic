{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.HeaderBar
(
    GarlicHeader,
    addRecipeClick,
    addRecipeToggle,
    headerBar,
)
where

import Garlic.Types
import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Reactive.Banana.GI.Gtk
import GI.Gtk

uiHeaderBar :: Text
uiHeaderBar = decodeUtf8 $(embedFile "res/headerbar.ui")

data GarlicHeader = GarlicHeader
    { _addRecipeClick  :: Event ()
    , _addRecipeToggle :: Consumer ()
    }

headerBar :: ApplicationWindow -> Garlic GarlicHeader
headerBar win = do
    b <- builderNew
    _ <- builderAddFromString b uiHeaderBar (-1)

    hb <- castB b "headerBar" HeaderBar
    windowSetTitlebar win (Just hb)

    addButton <- castB b "addButton" Button

    lift $ GarlicHeader
       <$> signalE0 addButton #clicked
       <*> pure (toggle addButton)

toggle :: Button -> Consumer a
toggle button = ioConsumer $ \_ -> do
    vis <- get button #visible
    set button [ #visible := not vis ]

makeGetters ''GarlicHeader
