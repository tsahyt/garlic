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
import Reactive.Banana

import Garlic.Types

trackingP :: GarlicApp -> Garlic ()
trackingP app = do
    active <- stepper FoodLog $ app ^. appVTracking . trackingSwitch

    goalsP 
        (app ^. appVTracking . trackingGoals)
        ((== Goals) <$> active)
        (app ^. appVTracking . trackingMarks)
        (app ^. appVTracking . trackingDate)
        (app ^. appStartup)
    weightLogP
        (app ^. appVTracking . trackingWeightLog)
        ((== WeightLog) <$> active)
        (app ^. appStartup)
        (app ^. appVTracking . trackingMarks)
        (app ^. appVTracking . trackingDate)
