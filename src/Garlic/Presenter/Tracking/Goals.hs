{-# LANGUAGE OverloadedStrings #-}
module Garlic.Presenter.Tracking.Goals
(
    goalsP
) 
where

import Control.Lens
import Data.Text (Text, pack)
import Data.Time
import Data.Time.Clock.POSIX
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Types
import Garlic.View.Tracking.Goals
import Garlic.Data.Units
import Reactive.Banana

import qualified Data.Map as M

goalsP ::
       GarlicTrackingGoals
    -> Behavior Bool
    -> Consumer [Day]
    -> Behavior Day
    -> Event ()
    -> Garlic (Behavior Goal)
goalsP gs active mark day startup = do
    activated <- filterE (== True) <$> plainChanges active

    let lbls = gs ^. tgLabels
        time = flip UTCTime 0 <$> day

    -- Input data
    let goal = currentGoal gs time

    -- Set grams for percentages
    consume (lbls ^. tglSetProteinGrams) . fmap goalProtein 
        =<< plainChanges goal
    consume (lbls ^. tglSetCarbsGrams) . fmap goalCarbs 
        =<< plainChanges goal
    consume (lbls ^. tglSetSugarsGrams) . fmap goalSugar 
        =<< plainChanges goal
    consume (lbls ^. tglSetFatsGrams) . fmap goalFat 
        =<< plainChanges goal
    consume (lbls ^. tglSetMacroSumVal) . fmap macroSum
        =<< plainChanges goal

    -- Legality
    sumChanged <- plainChanges $ macroPctSum gs
    lbls ^. tglSetMacroSumPct `consume` macroSumPctText <$> sumChanged

    -- Saving    
    addGoal `consume` goal <@ gs ^. tgSave
    
    -- Deletion
    deleteGoal `consume` time <@ gs ^. tgDelete

    -- Load existing goal on selection
    goalsE <- fetch getGoals $ unionl 
                  [ startup, gs ^. tgSave, gs ^. tgDelete, () <$ activated ]
    goalsB <- stepper M.empty goalsE
    current <- plainChanges $ goalAt <$> day <*> goalsB

    gs ^. tgLoadGoal `consume` current

    -- Load calendar marks
    mark `consume` whenE active (M.keys <$> goalsE)

    -- Determine current goal
    pure $ goalAt <$> day <*> goalsB

goalAt :: Day -> M.Map Day Goal -> Goal
goalAt today m = maybe defaultGoal snd $ M.lookupLE today m
  where
    defaultGoal =
        Goal
        { goalTimestamp = posixSecondsToUTCTime 0
        , goalKcal = 2000
        , goalProtein = 150
        , goalCarbs = 175
        , goalSugar = 52.5
        , goalFat = 77.8
        , goalSatFat = 37.8
        , goalMonoFat = 20
        , goalPolyFat = 20
        , goalSodium = 2000
        , goalCholesterol = 500
        , goalWeight = 75
        , goalUnit = Kilogram
        }

currentGoal ::
       GarlicTrackingGoals
    -> Behavior UTCTime
    -> Behavior Goal
currentGoal gs time =
    let protein = macro 4 <$> gs ^. tgKcal <*> gs ^. tgProtein
        carbs = macro 4 <$> gs ^. tgKcal <*> gs ^. tgCarbs
        sugar = (*) <$> carbs <*> gs ^. tgSugars
        fat = macro 9 <$> gs ^. tgKcal <*> gs ^. tgFat
        sat = (*) <$> fat <*> gs ^. tgSatUnsat
        unsat = (-) <$> fat <*> sat
        mono = (*) <$> unsat <*> gs ^. tgMonoPoly
        poly = (-) <$> unsat <*> mono
    in Goal 
           <$> time <*> gs ^. tgKcal <*> protein <*> carbs <*> sugar 
           <*> fat <*> sat <*> mono <*> poly <*> gs ^. tgSodium 
           <*> gs ^. tgCholesterol <*> gs ^. tgWeight <*> gs ^. tgUnit
  where
    macro f k p = k * p / f

macroSum :: Goal -> Double
macroSum g = goalProtein g + goalCarbs g + goalFat g

macroPctSum :: GarlicTrackingGoals -> Behavior Int
macroPctSum gs =
    let macros = sequenceA [gs ^. tgProtein, gs ^. tgCarbs, gs ^. tgFat]
    in sum . map (truncate . (* 100)) <$> macros

macroSumPctText :: Int -> (Legality, Text)
macroSumPctText 100 = (Legal, "100%")
macroSumPctText n   = (Illegal, pack (show n) `mappend` "%")
