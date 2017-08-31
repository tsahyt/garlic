{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.HeaderBar
(
    GarlicHeader,
    addClick,
    editClick,
    searchToggled,
    addRecipeToggle,
    yieldChanged,
    yieldToggle,
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
    { _addClick        :: Event ()
    , _editClick       :: Event ()
    , _searchToggled   :: Event ()
    , _yieldChanged    :: Event Double
    , _addRecipeToggle :: Consumer ()
    , _yieldToggle     :: Consumer ()
    }

headerBar :: ApplicationWindow -> Garlic GarlicHeader
headerBar win = do
    b <- builderNew
    _ <- builderAddFromString b uiHeaderBar (-1)

    hb <- castB b "headerBar" HeaderBar
    windowSetTitlebar win (Just hb)

    addButton <- castB b "addButton" Button
    searchButton <- castB b "searchButton" ToggleButton
    editButton <- castB b "editButton" Button
    yieldSpinner <- castB b "yieldSpinner" SpinButton
    yieldAdjustment <- castB b "yieldAdjustment" Adjustment

    lift $ GarlicHeader
       <$> signalE0 addButton #clicked
       <*> signalE0 editButton #clicked
       <*> signalE0 searchButton #toggled
       <*> attrE yieldAdjustment #value
       <*> pure (toggle yieldSpinner)
       <*> pure (toggle addButton)

toggle :: IsWidget w => w -> Consumer a
toggle w = ioConsumer $ \_ -> do
    vis <- widgetGetVisible w
    widgetSetVisible w (not vis)

makeGetters ''GarlicHeader
