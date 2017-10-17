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
nutritionP nt reload goal = do
    consume (nt ^. nLoadGoals) =<< plainChanges goal

    loaded <- fetch getNutritionSummary (flip UTCTime 0 <$> reload)
    let nsum = toNSum <$> filterE (not . null) loaded

    nt ^. nLoadNutrition `consume` nsum

toNSum :: [(Double, [WeighedIngredient])] -> NutritionSummary
toNSum = labelToSum . foldMap go
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
