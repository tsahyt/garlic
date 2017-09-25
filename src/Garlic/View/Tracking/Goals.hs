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
    tgLabels,

    goals,

    GarlicTrackingGoalsLabels,
    Legality (..),
    tglSetMacroSumPct,
    tglSetMacroSumVal
)
where

import Control.Monad.Trans
import GI.Gtk hiding (Unit)
import Data.Text (Text, pack)
import Reactive.Banana.GI.Gtk
import Reactive.Banana (stepper)
import Garlic.Types
import Garlic.Model (NutritionGoal (..))
import Garlic.Data.Units
import Text.Printf

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
    , _tgLabels        :: GarlicTrackingGoalsLabels
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
    unit        <- castB b "goalWeightUnit" ComboBoxText

    GarlicTrackingGoals 
       <$> lift (attrB kcal #value)
       <*> lift (((/ 100) <$$> attrB protein #value))
       <*> lift (((/ 100) <$$> attrB carbs #value))
       <*> lift (((/ 100) <$$> attrB sugars #value))
       <*> lift (((/ 100) <$$> attrB fat #value))
       <*> lift (((/ 100) <$$> attrB satUnsat #value))
       <*> lift (((/ 100) <$$> attrB monoPoly #value))
       <*> lift (attrB sodium #value)
       <*> lift (attrB cholesterol #value)
       <*> lift (attrB weight #value)
       <*> unitB unit
       <*> pure (ioConsumer (const $ return ()))
       <*> pure (ioConsumer (const $ return ()))
       <*> labels b

unitB :: ComboBoxText -> Garlic (Behavior Unit)
unitB c = do
    e <- lift (signalEN c #changed $ \h -> comboBoxTextGetActiveText c >>= h)
    stepper Kilogram $ parseUnit <$> e

data Legality = Illegal | Legal
    deriving (Eq, Show)

legalColor :: Legality -> Text
legalColor Illegal = "#a40000"
legalColor Legal = "#4e4e9a9a0606"

legalLabelText :: Bool -> Text -> Legality -> Text
legalLabelText bold content legal =
    pack $
    printf
        "<span %s color=\"%s\">%s</span>"
        (if bold
             then "weight=\"bold\""
             else [])
        (legalColor legal)
        content

data GarlicTrackingGoalsLabels = GarlicTrackingGoalsLabels
    { _tglSetMacroSumPct :: Consumer (Legality, Text)
    , _tglSetMacroSumVal :: Consumer Text
    }

labels :: Builder -> Garlic GarlicTrackingGoalsLabels
labels b = do
    macroSumPct <- castB b "goalMacroSumPct" Label
    macroSumVal <- castB b "goalMacroSumVal" Label

    GarlicTrackingGoalsLabels 
        <$> pure (ioConsumer $ \(l,t) -> 
                    labelSetMarkup macroSumPct (legalLabelText True t l))
        <*> pure (ioConsumer $ labelSetText macroSumVal)

-- LENSES
makeGetters ''GarlicTrackingGoals
makeGetters ''GarlicTrackingGoalsLabels
