module Garlic.Presenter.Tracking
(
    trackingP
) 
where

import Control.Lens

import Garlic.View
import Garlic.View.Tracking
import Garlic.Presenter.Tracking.Goals

import Garlic.Types

trackingP :: GarlicApp -> Garlic ()
trackingP app = do
    goalsP (app ^. appVTracking . trackingGoals)

    consume stdout . fmap show =<< plainChanges (app ^. appVTracking . trackingDate)
