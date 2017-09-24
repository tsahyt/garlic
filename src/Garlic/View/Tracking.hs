{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.View.Tracking 
(
    GarlicTracking,
    trackingFoodLog,
    trackingWeightLog,
    trackingNutrition,
    trackingGoals,
    viewTracking,

    GarlicTrackingView (..),
) 
where

import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.FileEmbed
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import GI.Gtk
import Reactive.Banana.GI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

import Garlic.Types

uiViewTracking :: Text
uiViewTracking = decodeUtf8 $(embedFile "res/view-tracking.ui")

data GarlicTrackingView
    = FoodLog
    | WeightLog
    | Nutrition
    | Goals
    deriving (Eq, Show)

data GarlicTracking = GarlicTracking
    { _trackingFoodLog   :: GarlicFoodLog
    , _trackingWeightLog :: GarlicWeightLog
    , _trackingNutrition :: GarlicNutrition
    , _trackingGoals     :: GarlicGoals 
    , _trackingSwitch    :: Event GarlicTrackingView
    }

viewTracking :: Stack -> Garlic GarlicTracking
viewTracking stack = do
    b <- builderNew
    _ <- builderAddFromString b uiViewTracking (-1)

    box <- castB b "mainBox" Box

    stackAddTitled stack box "view-tracking" "Tracking"

    switched <- viewSwitch b

    pure $ GarlicTracking
        GarlicFoodLog
        GarlicWeightLog
        GarlicNutrition
        GarlicGoals
        switched

viewSwitch :: Builder -> Garlic (Event GarlicTrackingView)
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

data GarlicFoodLog = GarlicFoodLog
data GarlicWeightLog = GarlicWeightLog
data GarlicNutrition = GarlicNutrition
data GarlicGoals = GarlicGoals

-- LENSES
makeGetters ''GarlicTracking
