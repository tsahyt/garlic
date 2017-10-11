{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
module Garlic.Data.Nutrition
(
    ReferencePerson (..),
    defaultReferencePerson,
    NVec (..),
    NutritionLabel (..),
    getNutrition,
    toLabel,
)
where

import Garlic.Model
import Garlic.Model.Queries
import Database.Persist (entityVal)
import Data.Text (Text)
import Data.Monoid
import Control.Lens
import Data.Maybe (fromMaybe)
import Garlic.Data.Units
import Linear.Vector
import Linear.V2

-- | Non-strict two dimensional vector
data NVec a = NVec a a
    deriving (Show, Eq, Functor)

instance Applicative NVec where
    pure x  = NVec x x
    NVec f g <*> NVec a b = NVec (f a) (g b)

instance Additive NVec where
    zero = NVec 0 0

instance R1 NVec where
    _x = lens (\(NVec a _) -> a) (\(NVec _ b) x -> NVec x b)

data NutritionLabel a = NutritionLabel
    { nlServing     :: !Text
    , nlKcal        :: a
    , nlKcalFat     :: a
    , nlFat         :: (NVec a)
    , nlSatFat      :: (NVec a)
    , nlTransFat    :: (NVec a)
    , nlCholesterol :: (NVec a)
    , nlSodium      :: (NVec a)
    , nlCarbs       :: (NVec a)
    , nlFibre       :: (NVec a)
    , nlSugars      :: (NVec a)
    , nlProtein     :: (NVec a)
    }
    deriving (Show, Eq, Functor)

instance Additive NutritionLabel where
    zero = NutritionLabel "" 0 0 zero zero zero zero zero zero zero zero zero
    liftU2 = liftI2
    liftI2 f a b = NutritionLabel
        { nlServing     = nlServing a <> nlServing b
        , nlKcal        = nlKcal a `f` nlKcal b
        , nlKcalFat     = nlKcalFat a `f` nlKcalFat b
        , nlFat         = liftI2 f (nlFat a) (nlFat b)
        , nlSatFat      = liftI2 f (nlSatFat a) (nlSatFat b)
        , nlTransFat    = liftI2 f (nlTransFat a) (nlTransFat b)
        , nlCholesterol = liftI2 f (nlCholesterol a) (nlCholesterol b)
        , nlSodium      = liftI2 f (nlSodium a) (nlSodium b)
        , nlCarbs       = liftI2 f (nlCarbs a) (nlCarbs b)
        , nlFibre       = liftI2 f (nlFibre a) (nlFibre b)
        , nlSugars      = liftI2 f (nlSugars a) (nlSugars b)
        , nlProtein     = liftI2 f (nlProtein a) (nlProtein b)
        }

instance Num a => Monoid (NutritionLabel a) where
    mempty = zero
    mappend = (^+^)

data ReferencePerson = ReferencePerson
    { referenceKcal    :: Double
    , referenceFat     :: Double
    , referenceCarbs   :: Double
    , referenceProtein :: Double
    }
    deriving (Show, Eq)

-- | The default reference person as declared by the US government
defaultReferencePerson :: ReferencePerson
defaultReferencePerson = ReferencePerson
    { referenceKcal = 2000
    , referenceFat = 78
    , referenceCarbs = 275
    , referenceProtein = 50
    }

getNutrition 
    :: Foldable t
    => ReferencePerson 
    -> Recipe 
    -> t WeighedIngredient
    -> NutritionLabel Double
getNutrition ref r is = 
    let x = (recip $ recipeYield r) *^ foldMap (toLabel ref) is
     in x { nlServing = recipeYieldUnit r }

toLabel :: ReferencePerson -> WeighedIngredient -> NutritionLabel Double
toLabel ref w = x
    where factor = fromMaybe 0 $
              convert (ingredientBasicUnit i)
                      (w ^. wingrAmount / ingredientBasicAmount i)
                      (w ^. wingrUnit)
          i = w ^. wingrIngr . to entityVal

          x = NutritionLabel
              { nlServing  = 
                    ""
              , nlKcal     = 
                    4 * (nlCarbs x ^. _x + nlProtein x ^. _x) + nlKcalFat x
              , nlKcalFat  = 
                    9 * nlFat x ^. _x
              , nlFat      = NVec 
                    (factor * ingredientFat i)
                    (nlFat x ^. _x / referenceFat ref)
              , nlSatFat   = NVec 
                    (factor * fromMaybe 0 (ingredientSatFat i)) 0
              , nlTransFat = NVec 
                    (factor * fromMaybe 0 (ingredientTransFat i)) 0
              , nlCholesterol = NVec
                    (factor * fromMaybe 0 (ingredientCholesterol i)) 0
              , nlSodium = NVec
                    (factor * fromMaybe 0 (ingredientSodium i)) 0
              , nlCarbs    = NVec 
                    (factor * ingredientCarbs i)
                    (nlCarbs x ^. _x / referenceCarbs ref)
              , nlFibre    = NVec 
                    (factor * fromMaybe 0 (ingredientFibre i)) 0
              , nlSugars   = NVec 
                    (factor * fromMaybe 0 (ingredientSugar i)) 0
              , nlProtein  = NVec 
                    (factor * ingredientProtein i)
                    (nlProtein x ^. _x / referenceProtein ref)
              }
