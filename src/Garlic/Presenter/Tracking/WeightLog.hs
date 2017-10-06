module Garlic.Presenter.Tracking.WeightLog
(
    weightLogP
) 
where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad
import Data.Text (Text, pack)
import Data.Time
import Garlic.Model
import Garlic.Data.Units
import Database.Persist (entityVal)
import Reactive.Banana
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.WeightLog

weightLogP :: GarlicTrackingWeightLog -> Event () -> Behavior Day -> Garlic ()
weightLogP wl startup day = do
    ok' <- (delay <=< delay) $ wl ^. wlOk
    let load = unionl [ () <$ wl ^. wlShowTime, ok', startup ]
    measurements <- fetch getWeightMeasurements load

    -- load data for graph
    wl ^. wlReloadMeasurements `consume` 
        map entityVal <$> measurements

    -- adding on OK
    addWeightMeasurement `consume` 
        currentMeasurement day (wl ^. wlInput) <@ wl ^. wlOk

    -- deletion
    deleteWeightMeasurement `consume`
        dayStamp day <@ wl ^. wlDelete

currentMeasurement ::
       Behavior Day -> Behavior (Double, Unit) -> Behavior WeightMeasurement
currentMeasurement day input =
    let weight = fst <$> input
        unit = snd <$> input
        timestamp = dayStamp day
    in WeightMeasurement <$> timestamp <*> weight <*> unit

dayStamp :: Behavior Day -> Behavior UTCTime
dayStamp day = UTCTime <$> day <*> pure 0
