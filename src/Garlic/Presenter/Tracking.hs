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

import Garlic.Presenter.Tracking.Goals

import Garlic.Types

trackingP :: GarlicApp -> Garlic ()
trackingP app =
    goalsP (app ^. appVTracking . trackingGoals)
