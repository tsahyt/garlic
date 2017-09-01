module Garlic.Presenter where

import Control.Lens
import Garlic.Model
import Garlic.Types
import Reactive.Banana
import GI.Gtk (Application)

import Garlic.View
import Garlic.View.HeaderBar

presenter :: Application -> Garlic ()
presenter app' = do
    app <- application app'

    app ^. appEnableSearch `consume` app ^. appHeader . searchToggled

    return ()
