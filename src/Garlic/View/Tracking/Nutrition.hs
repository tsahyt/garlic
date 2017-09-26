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
    nutrition,
) 
where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.IORef
import Data.Maybe
import Garlic.Types
import Garlic.Data.Units
import Data.Text (pack)
import Text.Printf
import GI.Gtk hiding (Unit)
import Reactive.Banana
import Reactive.Banana.GI.Gtk
import Garlic.Model (NutritionGoal (..))
import Garlic.View.Charts

import Graphics.Rendering.Cairo
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

data GarlicTrackingNutrition = GarlicTrackingNutrition
    { _nLoadNutrition :: Consumer NutritionSummary
    , _nLoadGoals :: Consumer NutritionGoal
    , _nLoadPast :: Consumer [NutritionSummary]
    }

nutrition :: Builder -> Garlic GarlicTrackingNutrition
nutrition b = do
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

    let loadValues NutritionSummary{..} = mapM_ (uncurry setGrams)
            [ (proteinValue, nsumProtein)
            , (carbsValue, nsumCarbs), (sugarsValue, nsumSugars)
            , (fibreValue, nsumFibre), (fatValue, nsumFat)
            , (satFatValue, nsumSatFat), (polyFatValue, nsumPolyFat)
            , (monoFatValue, nsumPolyFat), (transFatValue, nsumTransFat)
            , (cholesterolValue, nsumCholesterol), (sodiumValue, nsumSodium) ]

    goals <- liftIO $ newIORef Nothing
    proteinLevel <- castB b "nutritionProteinLevel" LevelBar
    carbsLevel <- castB b "nutritionCarbsLevel" LevelBar
    sugarsLevel <- castB b "nutritionSugarLevel" LevelBar
    fatLevel <- castB b "nutritionFatLevel" LevelBar
    satFatLevel <- castB b "nutritionSatFatLevel" LevelBar
    polyFatLevel <- castB b "nutritionPolyFatLevel" LevelBar
    monoFatLevel <- castB b "nutritionMonoFatLevel" LevelBar
    cholesterolLevel <- castB b "nutritionCholesterolLevel" LevelBar
    sodiumLevel <- castB b "nutritionSodiumLevel" LevelBar

    let loadLevels NutritionSummary{..} = do
            r <- readIORef goals
            case r of
                Nothing -> return ()
                Just NutritionGoal{..} -> mapM_ (uncurry levelBarSetValue)
                    [ (proteinLevel, nsumProtein / nutritionGoalProtein)
                    , (carbsLevel, nsumCarbs / nutritionGoalCarbs)
                    , (sugarsLevel, nsumSugars / nutritionGoalSugar)
                    , (fatLevel, nsumFat / nutritionGoalFat)
                    , (satFatLevel, nsumSatFat / nutritionGoalFat)
                    , (polyFatLevel, nsumPolyFat / nutritionGoalPolyFat)
                    , (monoFatLevel, nsumMonoFat / nutritionGoalMonoFat)
                    , (cholesterolLevel, nsumCholesterol / nutritionGoalCholesterol)
                    , (sodiumLevel, nsumSodium / nutritionGoalSodium) ]

    pieDA <- castB b "nutrientPie" DrawingArea
    summary <- pieChart pieDA

    historyDA <- castB b "nutrientPast" DrawingArea
    history <- historyChart historyDA

    pure $ GarlicTrackingNutrition
            (ioConsumer $ \x -> writeIORef summary (Just x) >> loadValues x >> loadLevels x)
            (ioConsumer $ writeIORef goals . Just)
            (ioConsumer $ \_ -> return ()) -- TODO: Chart

setGrams :: Label -> Double -> IO ()
setGrams l g =
    let t = pack $ printf "%.1fg" g
     in labelSetText l t

pieChart :: MonadIO m => DrawingArea -> m (IORef (Maybe NutritionSummary))
pieChart da = do
    ref <- liftIO $ newIORef Nothing
    _ <- on da #draw $ \ctx -> do
            w <- fromIntegral <$> widgetGetAllocatedWidth da
            h <- fromIntegral <$> widgetGetAllocatedHeight da
            dat <- readIORef ref

            let p = maybe 30 nsumProtein dat
                c = maybe 35 nsumCarbs dat
                f = maybe 20 nsumFat dat

            renderWithContext ctx $
                runCairo (w,h) (chartMacros p c f)
            pure False

    pure ref

historyChart :: MonadIO m => DrawingArea -> m (IORef [(UTCTime, Double)])
historyChart da = do
    ref <- liftIO $ newIORef []
    _ <- on da #draw $ \ctx -> do
            w <- fromIntegral <$> widgetGetAllocatedWidth da
            h <- fromIntegral <$> widgetGetAllocatedHeight da
            dat <- readIORef ref

            renderWithContext ctx $
                runCairo (w,h) (chartPastIntake dat)
            pure False

    pure ref

-- LENSES
makeGetters ''GarlicTrackingNutrition
