module Garlic.Presenter.Tracking.WeightLog
(
    weightLogP
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
import Garlic.View.Tracking.WeightLog

weightLogP ::
       GarlicTrackingWeightLog
    -> Behavior Bool
    -> Event ()
    -> Consumer [Day]
    -> Behavior Day
    -> Garlic ()
weightLogP wl active startup mark day = do
    activated <- filterE (== True) <$> plainChanges active
    now <- liftIO getCurrentTime
    let load = 
            timeFrameEnd now <$> 
                unionl [ wl ^. wlShowTime, TimeAll <$ startup
                       , TimeAll <$ activated ]
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

    -- calendar marks
    let entries = (utctDay . weightMeasurementTimestamp . entityVal) <$$> 
                        measurements
    mark `consume` whenE active entries

    -- set input on date select
    dateSel <- plainChanges day
    measurementsB <- stepper [] (map entityVal <$> measurements)
    let select = flip selectMeasure <$> measurementsB
        selected = select <@> dateSel
    wl ^. wlSetInput `consume` measureInput <$> selected
    wl ^. wlSetSelected `consume` selected

currentMeasurement ::
       Behavior Day -> Behavior (Double, Unit) -> Behavior WeightMeasurement
currentMeasurement day input =
    let weight = fst <$> input
        unit = snd <$> input
        timestamp = dayStamp day
    in WeightMeasurement <$> timestamp <*> weight <*> unit

selectMeasure :: Day -> [WeightMeasurement] -> Maybe WeightMeasurement
selectMeasure d = find (\x -> utctDay (weightMeasurementTimestamp x) == d)

measureInput :: Maybe WeightMeasurement -> (Double, Unit)
measureInput Nothing = (0, Kilogram)
measureInput (Just x) = (weightMeasurementWeight x, weightMeasurementUnit x)

timeFrameEnd :: UTCTime -> TimeFrame -> UTCTime
timeFrameEnd _ TimeAll = posixSecondsToUTCTime 0
timeFrameEnd t Time1Y  = addUTCTime (-31557600) t
timeFrameEnd t Time3M  = addUTCTime (-8035200) t
timeFrameEnd t Time1M  = addUTCTime (-2678400) t

dayStamp :: Behavior Day -> Behavior UTCTime
dayStamp day = UTCTime <$> day <*> pure 0
