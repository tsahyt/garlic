module Garlic.Presenter.Tracking.FoodLog
(
	foodLogP
) 
where

import Control.Lens
import Control.Monad.IO.Class
import Data.Time
import Data.Time.Clock.POSIX
import Data.List (find)
import Garlic.Model
import Garlic.Data.Units
import Database.Persist (entityVal)
import Reactive.Banana
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.FoodLog

foodLogP :: GarlicTrackingFoodLog -> Garlic ()
foodLogP fl = do
	pure ()
