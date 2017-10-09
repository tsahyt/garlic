{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
module Garlic.View.Tracking.WeightLog
(
    TimeFrame (..),
    GarlicTrackingWeightLog,
    wlReloadMeasurements,
    wlShowTime,
    wlInput,
    wlSetInput,
    wlSetSelected,
    wlDelete,
    wlOk,
    weightLog
) 
where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.IORef
import Garlic.Types
import Garlic.Data.Units
import GI.Gtk hiding (Unit)
import Reactive.Banana
import Reactive.Banana.GI.Gtk
import Garlic.Model (WeightMeasurement (..))
import Garlic.View.Charts

import Graphics.Rendering.Cairo.GI

data TimeFrame
    = TimeAll
    | Time1Y
    | Time3M
    | Time1M
    deriving (Eq, Show)

data GarlicTrackingWeightLog = GarlicTrackingWeightLog 
    { _wlReloadMeasurements :: Consumer [WeightMeasurement]
    , _wlShowTime           :: Event TimeFrame
    , _wlInput              :: Behavior (Double, Unit)
    , _wlSetInput           :: Consumer (Double, Unit)
    , _wlSetSelected        :: Consumer (Maybe WeightMeasurement)
    , _wlDelete             :: Event ()
    , _wlOk                 :: Event ()
    }

weightLog :: Builder -> Garlic GarlicTrackingWeightLog
weightLog b = do
    wAll <- castB b "weightAll" RadioButton
    w1Y  <- castB b "weight1Y" RadioButton
    w3M  <- castB b "weight3M" RadioButton
    w1M  <- castB b "weight1M" RadioButton

    tf <- unionl <$> sequence
              [ TimeAll <$$ lift (signalE0 wAll #toggled)
              , Time1Y  <$$ lift (signalE0 w1Y #toggled)
              , Time3M  <$$ lift (signalE0 w3M #toggled)
              , Time1M  <$$ lift (signalE0 w1M #toggled) ]

    delete <- castB b "weightDelete" Button
    ok <- castB b "weightOk" Button

    drawing <- castB b "weightChart" DrawingArea
    (refPts, refSel) <- weightChart drawing

    (input, setInput) <- measurement b

    GarlicTrackingWeightLog
        <$> pure (ioConsumer $ \x -> do
                      writeIORef refPts x
                      widgetQueueDraw drawing)
        <*> pure tf
        <*> pure input
        <*> pure setInput
        <*> pure (ioConsumer $ \x -> do
                      writeIORef refSel x
                      widgetQueueDraw drawing)
        <*> lift (signalE0 delete #clicked)
        <*> lift (signalE0 ok #clicked)

measurement ::
       Builder -> Garlic (Behavior (Double, Unit), Consumer (Double, Unit))
measurement b = do
    weight <- castB b "measurementAdjustment" Adjustment
    unit <- castB b "measurementUnit" ComboBoxText
    weightB <- lift $ attrB weight #value
    unitE <-
        lift $
        signalEN unit #changed $ \h -> comboBoxTextGetActiveText unit >>= h
    unitB <- stepper Kilogram $ parseUnit <$> unitE
    let bv = (,) <$> weightB <*> unitB
        cs =
            ioConsumer $ \(w, u) -> do
                adjustmentSetValue weight w
                case u of
                    Pound -> comboBoxSetActive unit 1
                    _ -> comboBoxSetActive unit 0
    pure (bv, cs)

weightChart ::
       MonadIO m
    => DrawingArea
    -> m (IORef [WeightMeasurement], IORef (Maybe WeightMeasurement))
weightChart da = do
    refPts <- liftIO $ newIORef []
    refSel <- liftIO $ newIORef Nothing
    _ <-
        on da #draw $ \ctx -> do
            w <- fromIntegral <$> widgetGetAllocatedWidth da
            h <- fromIntegral <$> widgetGetAllocatedHeight da
            pts <- readIORef refPts
            sel <- readIORef refSel
            renderWithContext ctx $ runCairo (w, h) (chartWeight sel pts)
            pure False
    pure (refPts, refSel)

-- LENSES
makeGetters ''GarlicTrackingWeightLog
