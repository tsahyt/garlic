{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.HeaderBar
(
    GarlicHeader,
    backClick,
    addClick,
    editClick,
    searchToggled,
    backToggle,
    addToggle,
    editToggle,
    yieldChanged,
    changeYield,
    yieldToggle,
    headerBar,
)
where

import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Garlic.Types
import Reactive.Banana.GI.Gtk

uiHeaderBar :: Text
uiHeaderBar = decodeUtf8 $(embedFile "res/headerbar.ui")

data GarlicHeader = GarlicHeader
    { _backClick       :: Event ()
    , _addClick        :: Event ()
    , _editClick       :: Event ()
    , _searchToggled   :: Event ()
    , _yieldChanged    :: Event Double
    , _changeYield     :: Consumer Double
    , _backToggle      :: Consumer ()
    , _addToggle       :: Consumer ()
    , _editToggle      :: Consumer ()
    , _yieldToggle     :: Consumer ()
    }

-- | Creates a new 'GarlicHeader' and sets the HeaderBar of the supplied
-- application window.
headerBar :: ApplicationWindow -> Garlic GarlicHeader
headerBar win = do
    b <- builderNew
    _ <- builderAddFromString b uiHeaderBar (-1)

    -- Widgets
    hb              <- castB b "headerBar" HeaderBar
    backButton      <- castB b "backButton" Button
    addButton       <- castB b "addButton" Button
    searchButton    <- castB b "searchButton" ToggleButton
    editButton      <- castB b "editButton" Button
    yieldSpinner    <- castB b "yieldSpinner" SpinButton
    yieldAdjustment <- castB b "yieldAdjustment" Adjustment

    -- Set the window title to the 'HeaderBar'
    windowSetTitlebar win (Just hb)

    lift $ GarlicHeader
       <$> signalE0 backButton #clicked
       <*> signalE0 addButton #clicked
       <*> signalE0 editButton #clicked
       <*> signalE0 searchButton #toggled
       <*> attrE yieldAdjustment #value
       <*> pure (ioConsumer $ \x -> set yieldAdjustment [ #value := x ])
       <*> pure (toggle backButton)
       <*> pure (toggle addButton)
       <*> pure (toggle editButton)
       <*> pure (toggle yieldSpinner)

-- | Toggle visibility of any widget.
--
-- TODO: Refactor into better place if useful.
toggle :: IsWidget w => w -> Consumer a
toggle w = ioConsumer $ \_ -> do
    vis <- widgetGetVisible w
    widgetSetVisible w (not vis)

makeGetters ''GarlicHeader
