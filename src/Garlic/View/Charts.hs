module Garlic.View.Charts
(
    runCairo,
    chartWeight,
    chartMacros,
    chartPastIntake
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

chartWeight :: 
       Maybe WeightMeasurement 
    -> [WeightMeasurement] 
    -> EC (Layout LocalTime Double) ()
chartWeight sel xs = do
    layout_background . fill_color .= transparent
    setColors [opaque orange, opaque red, opaque green]
    plot $ line "weight" [map (measurement Kilogram utc) xs]
    plot $ points "weight" (map (measurement Kilogram utc) xs)
    case sel of
        Nothing -> pure ()
        Just sel' ->
            plot $ points "selected" [measurement Kilogram utc sel']
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

chartPastIntake :: [(UTCTime, Double)] -> EC (Layout LocalTime Double) ()
chartPastIntake xs = do
    layout_legend .= Nothing
    layout_background . fill_color .= transparent
    setColors [opaque orange]
    plot $ fmap plotBars $ bars ["kcal"] (map go xs)
    return ()
  where
    go (t, v) = (utcToLocalTime utc t, [v])

runCairo :: ToRenderable r => (Double, Double) -> r -> Render ()
runCairo ext ec =
    void $
    runBackend (defaultEnv vectorAlignmentFns) (render (toRenderable ec) ext)
