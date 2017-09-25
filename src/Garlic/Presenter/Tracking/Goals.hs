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
    time <- pure <$> liftIO getCurrentTime
    let nutrient = nutrientGoal gs time
        weight   = weightGoal gs time

    -- Legality
    sumChanged <- plainChanges $ macroSum gs
    gs ^. tgLabels . tglSetMacroSumPct `consume` macroSumPctText <$> sumChanged

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

macroSum :: GarlicTrackingGoals -> Behavior Int
macroSum gs =
    let macros = sequenceA [gs ^. tgProtein, gs ^. tgCarbs, gs ^. tgFat]
    in sum . map (truncate . (* 100)) <$> macros

macroSumPctText :: Int -> (Legality, Text)
macroSumPctText 100 = (Legal, "100%")
macroSumPctText n   = (Illegal, pack (show n) `mappend` "%")

weightGoal :: GarlicTrackingGoals -> Behavior UTCTime -> Behavior WeightGoal
weightGoal gs time = WeightGoal <$> time <*> gs ^. tgWeight <*> gs ^. tgUnit
