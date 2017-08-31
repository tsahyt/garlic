{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Garlic.View
(
    -- * Application Framework
    GarlicApp,
    application
)
where

import Garlic.Types
import Control.Monad
import Control.Monad.Trans
import Control.Lens.Getter
import Reactive.Banana (Event)
import Reactive.Banana.GI.Gtk
import GI.Gtk

data GarlicApp = GarlicApp

application :: Application -> Garlic GarlicApp
application app = do
    win <- new ApplicationWindow []

    on app #activate $ do
        set win [ #application := app ]
        widgetShowAll win

    return GarlicApp

-- LENSES --
makeGetters ''GarlicApp
