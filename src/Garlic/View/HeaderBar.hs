{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.HeaderBar
(
    GarlicHeader,
    addClick,
    editClick,
    searchToggled,
    editToggle,
    yieldChanged,
    changeYield,
    yieldToggle,
    importIng,
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
    { _addClick        :: Event ()
    , _editClick       :: Event ()
    , _searchToggled   :: Event ()
    , _yieldChanged    :: Event Double
    , _changeYield     :: Consumer Double
    , _editToggle      :: Consumer ()
    , _yieldToggle     :: Consumer ()
    , _importIng       :: Event FilePath
    }

-- | Creates a new 'GarlicHeader' and sets the HeaderBar of the supplied
-- application window.
headerBar :: ApplicationWindow -> Garlic GarlicHeader
headerBar win = do
    b <- builderNew
    _ <- builderAddFromString b uiHeaderBar (-1)

    -- Widgets
    hb              <- castB b "headerBar" HeaderBar
    addButton       <- castB b "addButton" Button
    searchButton    <- castB b "searchButton" ToggleButton
    editButton      <- castB b "editButton" Button
    yieldSpinner    <- castB b "yieldSpinner" SpinButton
    yieldAdjustment <- castB b "yieldAdjustment" Adjustment
    importButton    <- castB b "importButton" Button

    -- Set the window title to the 'HeaderBar'
    windowSetTitlebar win (Just hb)

    impEv <- importIngredients importButton

    lift $ GarlicHeader
       <$> signalE0 addButton #clicked
       <*> signalE0 editButton #clicked
       <*> signalE0 searchButton #toggled
       <*> attrE yieldAdjustment #value
       <*> pure (ioConsumer $ \x -> set yieldAdjustment [ #value := x ])
       <*> pure (toggle editButton)
       <*> pure (toggle yieldSpinner)
       <*> pure impEv

-- | Toggle visibility of any widget.
--
-- TODO: Refactor into better place if useful.
toggle :: IsWidget w => w -> Consumer a
toggle w = ioConsumer $ \_ -> do
    vis <- widgetGetVisible w
    widgetSetVisible w (not vis)

importIngredients :: Button -> Garlic (Event FilePath)
importIngredients btn = lift $ signalEN btn #clicked $ \h -> do
    fc <- new FileChooserDialog []
    _  <- dialogAddButton fc "Import" 0
    _  <- dialogAddButton fc "Cancel" 1
    dialogSetDefaultResponse fc 0

    r <- dialogRun fc

    case r of
        0 -> fileChooserGetFilename fc >>= maybe (pure ()) h
        1 -> pure ()
        _ -> error "importIngredients: Invalid response"

    widgetDestroy fc

makeGetters ''GarlicHeader
