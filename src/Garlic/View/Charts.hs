module Garlic.View.Charts
(
    runCairo,
    chartWeight
)
where

import Control.Monad (void)
import Data.Maybe
import Data.Time
import Garlic.Data.Units
import Garlic.Model (WeightMeasurement(..))
import Graphics.Rendering.Cairo (Render)
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy

measurement :: Unit -> TimeZone -> WeightMeasurement -> (LocalTime, Double)
measurement u tz m =
    let w =
            fromMaybe 0 $
            convert u (weightMeasurementWeight m) (weightMeasurementUnit m)
        t = utcToLocalTime tz (weightMeasurementTimestamp m)
    in (t, w)

chartWeight :: [WeightMeasurement] -> EC (Layout LocalTime Double) ()
chartWeight xs = do
    setColors [opaque orange, opaque red]
    plot $ line "weight" [map (measurement Kilogram utc) xs]
    plot $ points "weight" (map (measurement Kilogram utc) xs)
    return ()

runCairo :: ToRenderable r => (Double, Double) -> r -> Render ()
runCairo ext ec =
    void $
    runBackend (defaultEnv vectorAlignmentFns) (render (toRenderable ec) ext)
