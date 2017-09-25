{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.Tracking.Goals
(
    goalsP
) 
where

import Control.Monad.IO.Class
import Control.Applicative
import Garlic.View.Tracking.Goals
import Data.Text (pack, Text)
import Garlic.View
import Garlic.Types
import Data.Time
import Garlic.Model
import Control.Lens

goalsP :: GarlicTrackingGoals -> Garlic ()
goalsP gs = do
    let lbls = gs ^. tgLabels

    -- TODO: proper time from calendar
    time <- pure <$> liftIO getCurrentTime

    -- Input data
    let nutrient = nutrientGoal gs time
        weight   = weightGoal gs time

    -- Set grams for percentages
    consume (lbls ^. tglSetProteinGrams) . fmap nutritionGoalProtein 
        =<< plainChanges nutrient
    consume (lbls ^. tglSetCarbsGrams) . fmap nutritionGoalCarbs 
        =<< plainChanges nutrient
    consume (lbls ^. tglSetSugarsGrams) . fmap nutritionGoalSugar 
        =<< plainChanges nutrient
    consume (lbls ^. tglSetFatsGrams) . fmap nutritionGoalFat 
        =<< plainChanges nutrient
    consume (lbls ^. tglSetMacroSumVal) . fmap macroSum
        =<< plainChanges nutrient

    -- Legality
    sumChanged <- plainChanges $ macroPctSum gs
    lbls ^. tglSetMacroSumPct `consume` macroSumPctText <$> sumChanged

    return ()

nutrientGoal ::
       GarlicTrackingGoals
    -> Behavior UTCTime
    -> Behavior NutritionGoal
nutrientGoal gs time =
    let protein = macro 4 <$> gs ^. tgKcal <*> gs ^. tgProtein
        carbs = macro 4 <$> gs ^. tgKcal <*> gs ^. tgCarbs
        sugar = (*) <$> carbs <*> gs ^. tgSugars
        fat = macro 9 <$> gs ^. tgKcal <*> gs ^. tgFat
        sat = (*) <$> fat <*> gs ^. tgSatUnsat
        unsat = (-) <$> fat <*> sat
        mono = (*) <$> unsat <*> gs ^. tgMonoPoly
        poly = (-) <$> unsat <*> mono
    in NutritionGoal 
           <$> time <*> gs ^. tgKcal <*> protein <*> carbs <*> sugar 
           <*> fat <*> sat <*> mono <*> poly <*> gs ^. tgSodium 
           <*> gs ^. tgCholesterol
  where
    macro f k p = k * p / f

macroSum :: NutritionGoal -> Double
macroSum g = nutritionGoalProtein g + nutritionGoalCarbs g + nutritionGoalFat g

macroPctSum :: GarlicTrackingGoals -> Behavior Int
macroPctSum gs =
    let macros = sequenceA [gs ^. tgProtein, gs ^. tgCarbs, gs ^. tgFat]
    in sum . map (truncate . (* 100)) <$> macros

macroSumPctText :: Int -> (Legality, Text)
macroSumPctText 100 = (Legal, "100%")
macroSumPctText n   = (Illegal, pack (show n) `mappend` "%")

weightGoal :: GarlicTrackingGoals -> Behavior UTCTime -> Behavior WeightGoal
weightGoal gs time = WeightGoal <$> time <*> gs ^. tgWeight <*> gs ^. tgUnit
