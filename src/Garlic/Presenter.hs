module Garlic.Presenter where

import Control.Lens
import Garlic.Model
import Garlic.View
import Garlic.Types
import Reactive.Banana

presenter :: Garlic ()
presenter = do
    app <- application
    consume stdout $ pure "activated!" <@ app ^. appActivate
