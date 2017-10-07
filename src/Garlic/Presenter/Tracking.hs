module Garlic.Presenter.Tracking
(
    trackingP
) 
where

import Control.Lens

import Garlic.View
import Garlic.View.Tracking
import Garlic.Presenter.Tracking.Goals
import Garlic.Presenter.Tracking.WeightLog

import Garlic.Types

trackingP :: GarlicApp -> Garlic ()
trackingP app = do
    goalsP (app ^. appVTracking . trackingGoals)
    weightLogP
        (app ^. appVTracking . trackingWeightLog)
        (app ^. appStartup)
        (app ^. appVTracking . trackingMarks)
        (app ^. appVTracking . trackingDate)
