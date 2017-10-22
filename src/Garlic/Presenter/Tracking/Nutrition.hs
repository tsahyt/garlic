{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Garlic.Presenter.Tracking.Nutrition 
(
    nutritionP
) 
where

import Control.Lens
import Data.Text (Text, pack)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Bifunctor
import Garlic.Model
import Garlic.Model.Queries
import Garlic.Data.Nutrition
import Garlic.Types
import Garlic.View.Tracking.Nutrition
import Garlic.Data.Units
import Reactive.Banana
import Linear.Vector
import Linear.V2

nutritionP :: GarlicTrackingNutrition -> Event Day -> Behavior Goal -> Garlic ()
nutritionP nt reload goal
 = do
    -- load goal changes
    consume (nt ^. nLoadGoals) =<< plainChanges goal

    -- load daily nutrition data for levels and values
    loaded <- fetch getNutritionSummary (flip UTCTime 0 <$> reload)
    let nsum = toNSum <$> loaded
    nt ^. nLoadNutrition `consume` nsum

    -- past for bar chart
    past <-
        stepper [] . fmap (map (second toNSum)) =<<
        fetch getPastNutrition (past7 <$> reload)
    nt ^. nLoadPast `consume` (extractPast <$> past) <@>
        unionl [ nt ^. nPastSelect, PastKcal <$ reload ]

past7 :: Day -> [UTCTime]
past7 d = map (\o -> UTCTime (addDays o d) 0) [-7..0]

extractPast ::
       [(UTCTime, Maybe NutritionSummary)] -> PastCategory -> [(UTCTime, Double)]
extractPast xs c = map (second go) xs
  where
    go Nothing = 0
    go (Just NutritionSummary{..}) =
        case c of
            PastKcal -> nsumKcal
            PastProtein -> nsumProtein
            PastCarbs -> nsumCarbs
            PastFat -> nsumFat

toNSum :: [(Double, [WeighedIngredient])] -> Maybe NutritionSummary
toNSum [] = Nothing
toNSum xs = Just . labelToSum . foldMap go $ xs
  where
    go (f, is) = f *^ foldMap (toLabel defaultReferencePerson) is

labelToSum :: NutritionLabel Double -> NutritionSummary
labelToSum NutritionLabel{..} = NutritionSummary
    { nsumKcal = nlKcal
    , nsumProtein = nlProtein ^. _x
    , nsumCarbs = nlCarbs ^. _x
    , nsumSugars = nlSugars ^. _x
    , nsumFibre = nlFibre ^. _x
    , nsumFat = nlFat ^. _x
    , nsumSatFat = nlSatFat ^. _x
    , nsumPolyFat = nlPolyFat ^. _x
    , nsumMonoFat = nlMonoFat ^. _x
    , nsumTransFat = nlTransFat ^. _x
    , nsumCholesterol = nlCholesterol ^. _x
    , nsumSodium = nlSodium ^. _x
    }
