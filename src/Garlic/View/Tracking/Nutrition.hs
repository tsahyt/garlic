{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
module Garlic.View.Tracking.Nutrition 
(
    NutritionSummary (..),
    PastCategory(..),
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
import Data.Maybe (fromMaybe)
import GI.Gtk hiding (Unit)
import Garlic.Model (Goal (..))
import Garlic.Types
import Garlic.View.Charts
import Garlic.Data.Units
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

nsumZero :: NutritionSummary
nsumZero = NutritionSummary 0 0 0 0 0 0 0 0 0 0 0 0

data PastCategory
    = PastKcal
    | PastProtein
    | PastCarbs
    | PastFat
    deriving (Show, Eq, Ord, Read)

data GarlicTrackingNutrition = GarlicTrackingNutrition
    { _nLoadNutrition :: Consumer (Maybe NutritionSummary)
    , _nLoadGoals :: Consumer Goal
    , _nLoadPast :: Consumer [(UTCTime, Double)]
    , _nPastSelect :: Event PastCategory
    }

nutrition :: Builder -> Garlic GarlicTrackingNutrition
nutrition b = do
    goals <- liftIO $ newIORef Nothing
    loadValues <- values b
    loadLevels <- levels b goals
    loadIntake <- topBarI b goals
    loadGoal <- topBarG b
    -- pie chart
    pieDA <- castB b "nutrientPie" DrawingArea
    summary <- pieChart pieDA
    -- history chart
    historyDA <- castB b "nutrientPast" DrawingArea
    history <- historyChart historyDA
    past <- pastSelect b
    pure $
        GarlicTrackingNutrition
            (ioConsumer $ \x -> do
                 writeIORef summary x
                 let x' = fromMaybe nsumZero x
                 loadValues x'
                 loadLevels x'
                 loadIntake x')
            (ioConsumer $ \g -> do
                 writeIORef goals (Just g)
                 loadGoal g)
            (ioConsumer $ \h -> do
                 writeIORef history h
                 widgetQueueDraw historyDA)
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

setUnit :: Unit -> Label -> Double -> IO ()
setUnit u l g =
    let t = pack $ printf "%.1f%s" g (prettyUnit u :: String)
    in labelSetText l t

topBarI ::
       MonadIO m
    => Builder
    -> IORef (Maybe Goal)
    -> m (NutritionSummary -> IO ())
topBarI b ref = do
    intake <- castB b "intakeValue" Label
    remaining <- castB b "remainingValue" Label
    intakeL <- castB b "intakeLevel" LevelBar
    pure $ \NutritionSummary {..} -> do
        g <- readIORef ref
        let pp x = pack (printf "%.0f" x)
            gk   = maybe 2000 goalKcal g 
        labelSetText intake (pp nsumKcal)
        labelSetText remaining (pp $ gk - nsumKcal)
        levelBarSetValue intakeL (nsumKcal / gk)

topBarG :: MonadIO m => Builder -> m (Goal -> IO ())
topBarG b = do
    goal <- castB b "goalValue" Label
    pure $ \g -> labelSetText goal (pack . show @Int . truncate . goalKcal $ g)

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
    pure $ \NutritionSummary {..} -> do
        mapM_
            (uncurry (setUnit Gram))
            [ (proteinValue, nsumProtein)
            , (carbsValue, nsumCarbs)
            , (sugarsValue, nsumSugars)
            , (fibreValue, nsumFibre)
            , (fatValue, nsumFat)
            , (satFatValue, nsumSatFat)
            , (polyFatValue, nsumPolyFat)
            , (monoFatValue, nsumPolyFat)
            , (transFatValue, nsumTransFat)
            ]
        mapM_
            (uncurry (setUnit Milligram))
            [ (cholesterolValue, nsumCholesterol)
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
                    [ (proteinLevel, min 1 $ nsumProtein / goalProtein)
                    , (carbsLevel, min 1 $ nsumCarbs / goalCarbs)
                    , (sugarsLevel, min 1 $ nsumSugars / goalSugar)
                    , (fatLevel, min 1 $ nsumFat / goalFat)
                    , (satFatLevel, min 1 $ nsumSatFat / goalFat)
                    , (polyFatLevel, min 1 $ nsumPolyFat / goalPolyFat)
                    , (monoFatLevel, min 1 $ nsumMonoFat / goalMonoFat)
                    , ( cholesterolLevel
                      , min 1 $ nsumCholesterol / goalCholesterol)
                    , (sodiumLevel, min 1 $ nsumSodium / goalSodium)
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
