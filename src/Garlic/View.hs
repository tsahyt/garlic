{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View
(
    -- * Application Framework
    GarlicApp,
    appActivate,
    appShutdown,
    appStartup,
    application
)
where

import Garlic.Types
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Reactive.Banana
import Reactive.Banana.GI.Gtk
import GI.Gtk

data GarlicApp = GarlicApp
    { _appActivate :: Event ()
    , _appShutdown :: Event ()
    , _appStartup  :: Event ()
    }

makeGetters ''GarlicApp

application :: Application -> Garlic GarlicApp
application app =
    lift $ GarlicApp
       <$> signalE0 app #activate
       <*> signalE0 app #shutdown
       <*> signalE0 app #startup
