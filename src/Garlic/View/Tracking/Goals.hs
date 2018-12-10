{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
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
    tgLoadGoal,
    tgLabels,
    tgSave,
    tgDelete,

    goals,

    GarlicTrackingGoalsLabels,
    Legality (..),
    tglSetMacroSumPct,
    tglSetMacroSumVal,
    tglSetProteinGrams,
    tglSetCarbsGrams,
    tglSetSugarsGrams,
    tglSetFatsGrams,
)
where

import Control.Monad.Trans
import GI.Gtk hiding (Unit)
import Data.Text (Text, pack)
import Reactive.Banana.GI.Gtk
import Reactive.Banana (stepper)
import Garlic.Types
import Garlic.Model (Goal (..))
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
    , _tgLoadGoal      :: Consumer Goal
    , _tgLabels        :: GarlicTrackingGoalsLabels
    , _tgSave          :: Event ()
    , _tgDelete        :: Event ()
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
    save        <- castB b "goalSave" Button
    delete      <- castB b "goalDelete" Button

    let unmacro f k x = (x * f / k) * 100
        loadGoals Goal{..} = do
            adjustmentSetValue kcal goalKcal
            adjustmentSetValue protein $ unmacro 4 goalKcal goalProtein
            adjustmentSetValue carbs $ unmacro 4 goalKcal goalCarbs
            adjustmentSetValue sugars $ 100 * goalSugar / goalCarbs
            adjustmentSetValue fat $ unmacro 9 goalKcal goalFat
            adjustmentSetValue satUnsat $ 100 * goalSatFat / goalFat
            adjustmentSetValue monoPoly $ 
                100 * goalMonoFat / (goalFat - goalSatFat)
            adjustmentSetValue sodium goalSodium
            adjustmentSetValue cholesterol goalCholesterol
            adjustmentSetValue weight goalWeight
            case goalUnit of
                Pound -> comboBoxSetActive unit 1
                _ -> comboBoxSetActive unit 0

    GarlicTrackingGoals 
       <$> lift (attrB kcal #value)
       <*> lift ((/ 100) <$$> attrB protein #value)
       <*> lift ((/ 100) <$$> attrB carbs #value)
       <*> lift ((/ 100) <$$> attrB sugars #value)
       <*> lift ((/ 100) <$$> attrB fat #value)
       <*> lift ((/ 100) <$$> attrB satUnsat #value)
       <*> lift ((/ 100) <$$> attrB monoPoly #value)
       <*> lift (attrB sodium #value)
       <*> lift (attrB cholesterol #value)
       <*> lift (attrB weight #value)
       <*> unitB unit
       <*> pure (ioConsumer loadGoals)
       <*> labels b
       <*> lift (signalE0 save #clicked)
       <*> lift (signalE0 delete #clicked)

unitB :: ComboBoxText -> Garlic (Behavior Unit)
unitB c = do
    e <- lift (signalEN c #changed (\h -> comboBoxTextGetActiveText c >>= h) ())
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
    { _tglSetMacroSumPct  :: Consumer (Legality, Text)
    , _tglSetMacroSumVal  :: Consumer Double
    , _tglSetProteinGrams :: Consumer Double
    , _tglSetCarbsGrams   :: Consumer Double
    , _tglSetSugarsGrams  :: Consumer Double
    , _tglSetFatsGrams    :: Consumer Double
    }

labels :: Builder -> Garlic GarlicTrackingGoalsLabels
labels b = do
    macroSumPct  <- castB b "goalMacroSumPct" Label
    macroSumVal  <- castB b "goalMacroSumVal" Label
    proteinGrams <- castB b "goalProteinGrams" Label
    carbsGrams   <- castB b "goalCarbsGrams" Label
    sugarsGrams  <- castB b "goalSugarsGrams" Label
    fatGrams     <- castB b "goalFatGrams" Label

    pure $ GarlicTrackingGoalsLabels 
               (ioConsumer $ \(l,t) -> 
                    labelSetMarkup macroSumPct (legalLabelText True t l))
               (ioConsumer $ setGrams macroSumVal)
               (ioConsumer $ setGrams proteinGrams)
               (ioConsumer $ setGrams carbsGrams)
               (ioConsumer $ setGrams sugarsGrams)
               (ioConsumer $ setGrams fatGrams)

setGrams :: Label -> Double -> IO ()
setGrams l g =
    let t = pack $ printf "%.1fg" g
     in labelSetText l t

-- LENSES
makeGetters ''GarlicTrackingGoals
makeGetters ''GarlicTrackingGoalsLabels
