{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Garlic.View.Tracking.Nutrition 
(
    NutritionSummary (..),
    GarlicTrackingNutrition,
    nLoadNutrition,
    nLoadPast,
    nLoadGoals,
    nPastSelect,
    nutrition,
) 
where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.IORef
import Data.Text (pack)
import GI.Gtk hiding (Unit)
import Garlic.Model (Goal (..))
import Garlic.Types
import Garlic.View.Charts
import Reactive.Banana.GI.Gtk
import Text.Printf

import Graphics.Rendering.Cairo.GI

import Data.Time

data NutritionSummary = NutritionSummary
    { nsumKcal :: Double
    , nsumProtein :: Double
    , nsumCarbs :: Double
    , nsumSugars :: Double
    , nsumFibre :: Double
    , nsumFat :: Double
    , nsumSatFat :: Double
    , nsumPolyFat :: Double
    , nsumMonoFat :: Double
    , nsumTransFat :: Double
    , nsumCholesterol :: Double
    , nsumSodium :: Double
    } deriving (Show, Eq)

data PastCategory
    = PastKcal
    | PastProtein
    | PastCarbs
    | PastFat
    deriving (Show, Eq, Ord, Read)

data GarlicTrackingNutrition = GarlicTrackingNutrition
    { _nLoadNutrition :: Consumer NutritionSummary
    , _nLoadGoals :: Consumer Goal
    , _nLoadPast :: Consumer [(UTCTime, Double)]
    , _nPastSelect :: Event PastCategory
    }

nutrition :: Builder -> Garlic GarlicTrackingNutrition
nutrition b = do
    goals <- liftIO $ newIORef Nothing
    loadValues <- values b
    loadLevels <- levels b goals
    -- pie chart
    pieDA <- castB b "nutrientPie" DrawingArea
    summary <- pieChart pieDA
    -- history chart
    historyDA <- castB b "nutrientPast" DrawingArea
    history <- historyChart historyDA
    past <- pastSelect b
    pure $
        GarlicTrackingNutrition
            (ioConsumer $ \x ->
                 writeIORef summary (Just x) >> loadValues x >> loadLevels x)
            (ioConsumer $ writeIORef goals . Just)
            (ioConsumer $ writeIORef history)
            past

pastSelect :: Builder -> Garlic (Event PastCategory)
pastSelect b = do
    kcal <- castB b "pastKcalRadio" RadioButton
    protein <- castB b "pastProteinRadio" RadioButton
    carbs <- castB b "pastCarbsRadio" RadioButton
    fat <- castB b "pastFatRadio" RadioButton
    unionl <$>
        sequence
            [ PastKcal <$$ lift (signalE0 kcal #toggled)
            , PastProtein <$$ lift (signalE0 protein #toggled)
            , PastCarbs <$$ lift (signalE0 carbs #toggled)
            , PastFat <$$ lift (signalE0 fat #toggled)
            ]

setGrams :: Label -> Double -> IO ()
setGrams l g =
    let t = pack $ printf "%.1fg" g
    in labelSetText l t

values :: MonadIO m => Builder -> m (NutritionSummary -> IO ())
values b = do
    proteinValue <- castB b "nutritionProteinValue" Label
    carbsValue <- castB b "nutritionCarbsValue" Label
    sugarsValue <- castB b "nutritionSugarValue" Label
    fibreValue <- castB b "nutritionFibreValue" Label
    fatValue <- castB b "nutritionFatValue" Label
    satFatValue <- castB b "nutritionSatFatValue" Label
    polyFatValue <- castB b "nutritionPolyFatValue" Label
    monoFatValue <- castB b "nutritionMonoFatValue" Label
    transFatValue <- castB b "nutritionTransFatValue" Label
    cholesterolValue <- castB b "nutritionCholesterolValue" Label
    sodiumValue <- castB b "nutritionSodiumValue" Label
    pure $ \NutritionSummary {..} ->
        mapM_
            (uncurry setGrams)
            [ (proteinValue, nsumProtein)
            , (carbsValue, nsumCarbs)
            , (sugarsValue, nsumSugars)
            , (fibreValue, nsumFibre)
            , (fatValue, nsumFat)
            , (satFatValue, nsumSatFat)
            , (polyFatValue, nsumPolyFat)
            , (monoFatValue, nsumPolyFat)
            , (transFatValue, nsumTransFat)
            , (cholesterolValue, nsumCholesterol)
            , (sodiumValue, nsumSodium)
            ]

levels ::
       MonadIO m
    => Builder
    -> IORef (Maybe Goal)
    -> m (NutritionSummary -> IO ())
levels b goals = do
    proteinLevel <- castB b "nutritionProteinLevel" LevelBar
    carbsLevel <- castB b "nutritionCarbsLevel" LevelBar
    sugarsLevel <- castB b "nutritionSugarLevel" LevelBar
    fatLevel <- castB b "nutritionFatLevel" LevelBar
    satFatLevel <- castB b "nutritionSatFatLevel" LevelBar
    polyFatLevel <- castB b "nutritionPolyFatLevel" LevelBar
    monoFatLevel <- castB b "nutritionMonoFatLevel" LevelBar
    cholesterolLevel <- castB b "nutritionCholesterolLevel" LevelBar
    sodiumLevel <- castB b "nutritionSodiumLevel" LevelBar
    pure $ \NutritionSummary {..} -> do
        r <- readIORef goals
        case r of
            Nothing -> return ()
            Just Goal {..} ->
                mapM_
                    (uncurry levelBarSetValue)
                    [ (proteinLevel, nsumProtein / goalProtein)
                    , (carbsLevel, nsumCarbs / goalCarbs)
                    , (sugarsLevel, nsumSugars / goalSugar)
                    , (fatLevel, nsumFat / goalFat)
                    , (satFatLevel, nsumSatFat / goalFat)
                    , (polyFatLevel, nsumPolyFat / goalPolyFat)
                    , (monoFatLevel, nsumMonoFat / goalMonoFat)
                    , ( cholesterolLevel
                      , nsumCholesterol / goalCholesterol)
                    , (sodiumLevel, nsumSodium / goalSodium)
                    ]

pieChart :: MonadIO m => DrawingArea -> m (IORef (Maybe NutritionSummary))
pieChart da = do
    ref <- liftIO $ newIORef Nothing
    _ <-
        on da #draw $ \ctx -> do
            (w,h) <- dimensions da
            dat <- readIORef ref
            let p = maybe 30 nsumProtein dat
                c = maybe 35 nsumCarbs dat
                f = maybe 20 nsumFat dat
            renderWithContext ctx $ runCairo (w, h) (chartMacros p c f)
            pure False
    pure ref

historyChart :: MonadIO m => DrawingArea -> m (IORef [(UTCTime, Double)])
historyChart da = do
    ref <- liftIO $ newIORef []
    _ <-
        on da #draw $ \ctx -> do
            (w,h) <- dimensions da
            dat <- readIORef ref
            renderWithContext ctx $ runCairo (w, h) (chartPastIntake dat)
            pure False
    pure ref

-- LENSES
makeGetters ''GarlicTrackingNutrition
