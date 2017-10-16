{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.Tracking.Nutrition 
(
    nutritionP
) 
where

import Control.Lens
import Data.Text (Text, pack)
import Data.Time
import Data.Time.Clock.POSIX
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.Nutrition
import Garlic.Data.Units
import Reactive.Banana

nutritionP :: GarlicTrackingNutrition -> Event () -> Behavior Goal -> Garlic ()
nutritionP nt reload goal = do
    consume (nt ^. nLoadGoals) =<< plainChanges goal
    pure ()
