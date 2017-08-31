{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View where

import Garlic.Types
import Control.Monad.Trans
import Control.Lens
import Reactive.Banana
import Reactive.Banana.GI.Gtk
import GI.Gtk
import GI.Gio (applicationRun)

data GarlicGUI = GarlicGUI
    { _appActivate :: Event ()
    , _appShutdown :: Event ()
    , _appStartup  :: Event ()
    }

makeLenses ''GarlicGUI

application :: Garlic GarlicGUI
application = do
    app <- new Application []
    applicationRun app Nothing

    lift $ GarlicGUI 
       <$> signalE0 app #activate
       <*> signalE0 app #shutdown
       <*> signalE0 app #startup
