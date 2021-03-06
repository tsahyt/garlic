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
    searchToggle,
    headerBar,
    mainView,

    MainView (..)
)
where

import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Garlic.Types
import Reactive.Banana.GI.Gtk
import Reactive.Banana.Frameworks
import Reactive.Banana (stepper)

uiHeaderBar :: Text
uiHeaderBar = decodeUtf8 $(embedFile "res/headerbar.ui")

data GarlicHeader = GarlicHeader
    { _backClick       :: Event ()
    , _addClick        :: Event ()
    , _editClick       :: Event ()
    , _searchToggled   :: Event ()
    , _yieldChanged    :: Event Double
    , _changeYield     :: Consumer Double
    , _backToggle      :: Consumer Bool
    , _addToggle       :: Consumer Bool
    , _editToggle      :: Consumer Bool
    , _yieldToggle     :: Consumer Bool
    , _searchToggle    :: Consumer Bool
    , _mainView        :: Behavior MainView
    }

-- | Creates a new 'GarlicHeader' and sets the HeaderBar of the supplied
-- application window.
headerBar :: ApplicationWindow -> Stack -> Garlic GarlicHeader
headerBar win stack = do
    b <- builderNew
    _ <- builderAddFromString b uiHeaderBar maxBound

    -- Widgets
    hb              <- castB b "headerBar" HeaderBar
    backButton      <- castB b "backButton" Button
    addButton       <- castB b "addButton" Button
    searchButton    <- castB b "searchButton" ToggleButton
    editButton      <- castB b "editButton" Button
    yieldSpinner    <- castB b "yieldSpinner" SpinButton
    yieldAdjustment <- castB b "yieldAdjustment" Adjustment
    switcher        <- castB b "switcher" StackSwitcher

    -- Set the window title to the 'HeaderBar'
    windowSetTitlebar win (Just hb)

    -- Connect Stack
    stackSwitcherSetStack switcher (Just stack)

    -- Switcher Event
    switch <- viewSwitch switcher stack

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
       <*> pure (toggle searchButton)
       <*> pure switch

-- | Toggle visibility of any widget.
toggle :: IsWidget w => w -> Consumer Bool
toggle w = ioConsumer $ widgetSetVisible w

data MainView
    = MainRecipes
    | MainTracking
    deriving (Eq, Show, Ord, Enum)

viewSwitch :: StackSwitcher -> Stack -> Garlic (Behavior MainView)
viewSwitch switcher stack = do
    (e, h) <- lift newEvent
    _ <- on switcher #buttonReleaseEvent $ \_ -> do
        name <- stackGetVisibleChildName stack
        case name of
            Just "view-recipes" -> h MainRecipes
            Just "view-tracking" -> h MainTracking
            _ -> pure ()
        return True

    stepper MainRecipes e

makeGetters ''GarlicHeader
