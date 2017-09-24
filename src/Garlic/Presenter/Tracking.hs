module Garlic.Presenter.Tracking
(
    trackingP
) 
where

import Control.Lens
import Reactive.Banana

import Garlic.View
import Garlic.View.Tracking
import Garlic.View.Tracking.Goals

import Garlic.Types

trackingP :: GarlicApp -> Garlic ()
trackingP app = do
    x <- plainChanges $ app ^. appVTracking . trackingGoals . tgKcal
    stdout `consume` show <$> x
