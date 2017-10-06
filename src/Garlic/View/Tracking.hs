{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.View.Tracking 
(
    GarlicViewTracking,
    trackingFoodLog,
    trackingWeightLog,
    trackingNutrition,
    trackingGoals,
    trackingSwitch,
    trackingDate,
    viewTracking,

    GarlicTrackingStack (..),
) 
where

import Control.Monad.Trans
import Data.FileEmbed
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Reactive.Banana.GI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

import Garlic.Types
import Garlic.View.Tracking.Goals
import Garlic.View.Tracking.WeightLog
import Garlic.View.Tracking.FoodLog
import Garlic.View.Tracking.Nutrition

import Data.Time.Calendar
import Data.Time.Clock

uiViewTracking :: Text
uiViewTracking = decodeUtf8 $(embedFile "res/view-tracking.ui")

data GarlicTrackingStack
    = FoodLog
    | WeightLog
    | Nutrition
    | Goals
    deriving (Eq, Show)

data GarlicViewTracking = GarlicViewTracking
    { _trackingFoodLog   :: GarlicTrackingFoodLog
    , _trackingWeightLog :: GarlicTrackingWeightLog
    , _trackingNutrition :: GarlicTrackingNutrition
    , _trackingGoals     :: GarlicTrackingGoals 
    , _trackingSwitch    :: Event GarlicTrackingStack
    , _trackingDate      :: Behavior Day
    }

viewTracking :: Stack -> Garlic GarlicViewTracking
viewTracking stack = do
    b <- builderNew
    _ <- builderAddFromString b uiViewTracking (-1)

    box <- castB b "mainBox" Box

    stackAddTitled stack box "view-tracking" "Tracking"

    switched <- viewSwitch b

    GarlicViewTracking
        <$> foodLog b
        <*> weightLog b
        <*> nutrition b
        <*> goals b
        <*> pure switched
        <*> calendar b

viewSwitch :: Builder -> Garlic (Event GarlicTrackingStack)
viewSwitch b = do
    sidebar <- castB b "sidebar" StackSidebar
    stack   <- castB b "trackingStack" Stack
    (e, h) <- lift $ newEvent

    _ <- on sidebar #buttonReleaseEvent $ \_ -> do
            name <- stackGetVisibleChildName stack
            case name of
                Just "foodLog" -> h FoodLog
                Just "weightLog" -> h WeightLog
                Just "nutrition" -> h Nutrition
                Just "goals" -> h Goals
                _ -> pure ()
            return True

    pure $ e

calendar :: Builder -> Garlic (Behavior Day)
calendar b = do
    cal <- castB b "calendar" Calendar

    now <- liftIO (utctDay <$> getCurrentTime)
    sel <- lift $ signalEN cal #daySelected $ \h -> do
                    (y,m,d) <- calendarGetDate cal
                    h (fromIntegral y, fromIntegral m + 1, fromIntegral d)

    stepper now $ (\(y,m,d) -> fromGregorian y m d) <$> sel
    
-- LENSES
makeGetters ''GarlicViewTracking
