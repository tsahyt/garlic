{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
module Garlic.View.Tracking.Goals
(
    GarlicTrackingGoals,
    tgKcal,
    tgProtein,
    tgCarbs,
    tgSugars,
    tgFat,
    tgSatUnsat,
    tgMonoPoly,
    tgSodium,
    tgCholesterol,
    tgWeight,
    tgUnit,
    tgLoadWeight,
    tgLoadNutrients,

    goals
)
where

import Control.Monad.Trans
import GI.Gtk hiding (Unit)
import Reactive.Banana.GI.Gtk
import Garlic.Types
import Garlic.Model (NutritionGoal (..))
import Garlic.Data.Units

data GarlicTrackingGoals = GarlicTrackingGoals
    { _tgKcal          :: Behavior Double
    , _tgProtein       :: Behavior Double
    , _tgCarbs         :: Behavior Double
    , _tgSugars        :: Behavior Double
    , _tgFat           :: Behavior Double
    , _tgSatUnsat      :: Behavior Double
    , _tgMonoPoly      :: Behavior Double
    , _tgSodium        :: Behavior Double
    , _tgCholesterol   :: Behavior Double
    , _tgWeight        :: Behavior Double
    , _tgUnit          :: Behavior Unit
    , _tgLoadWeight    :: Consumer (Double, Unit)
    , _tgLoadNutrients :: Consumer NutritionGoal
    }

goals :: Builder -> Garlic GarlicTrackingGoals
goals b = do
    kcal        <- castB b "goalKcalAdjustment" Adjustment
    protein     <- castB b "goalProteinAdjustment" Adjustment
    carbs       <- castB b "goalCarbsAdjustment" Adjustment
    sugars      <- castB b "goalSugarsAdjustment" Adjustment
    fat         <- castB b "goalFatAdjustment" Adjustment
    satUnsat    <- castB b "goalSatUnsatAdjustment" Adjustment
    monoPoly    <- castB b "goalMonoPolyAdjustment" Adjustment
    sodium      <- castB b "goalSodiumAdjustment" Adjustment
    cholesterol <- castB b "goalCholesterolAdjustment" Adjustment
    weight      <- castB b "goalWeightAdjustment" Adjustment

    lift $ GarlicTrackingGoals 
       <$> attrB kcal #value
       <*> attrB protein #value
       <*> attrB carbs #value
       <*> attrB sugars #value
       <*> attrB fat #value
       <*> attrB satUnsat #value
       <*> attrB monoPoly #value
       <*> attrB sodium #value
       <*> attrB cholesterol #value
       <*> attrB weight #value
       <*> pure (pure Kilogram)
       <*> pure (ioConsumer (const $ return ()))
       <*> pure (ioConsumer (const $ return ()))

-- LENSES
makeGetters ''GarlicTrackingGoals
