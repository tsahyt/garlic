module Garlic.View.Charts
(
    runCairo,
    chartWeight,
    chartMacros
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
    layout_background . fill_color .= transparent
    setColors [opaque orange, opaque red]
    plot $ line "weight" [map (measurement Kilogram utc) xs]
    plot $ points "weight" (map (measurement Kilogram utc) xs)
    return ()

chartMacros :: Double -> Double -> Double -> EC PieLayout ()
chartMacros protein carbs fat = do
    pie_background . fill_color .= transparent
    setColors [opaque blue, opaque green, opaque red]
    pie_plot . pie_data .=
        map pitem [("Protein", protein), ("Carbs", carbs), ("Fat", fat)]
    return ()
  where
    pitem (l, v) = pitem_value .~ v $ pitem_label .~ l $ def

runCairo :: ToRenderable r => (Double, Double) -> r -> Render ()
runCairo ext ec =
    void $
    runBackend (defaultEnv vectorAlignmentFns) (render (toRenderable ec) ext)
