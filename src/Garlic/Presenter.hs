module Garlic.Presenter where

import Control.Lens
import Control.Monad.IO.Class
import Garlic.Model
import Garlic.View
import Garlic.Types
import Reactive.Banana

import GI.Gtk (Application)

presenter :: Application -> Garlic ()
presenter app' = do
    app <- application app'

    {-
     -consume stdout $ pure "activated!" <@ app ^. appActivate
     -consume stdout $ pure "shutdown!" <@ app ^. appShutdown
     -consume stdout $ pure "startup!" <@ app ^. appStartup
     -}
    return ()
