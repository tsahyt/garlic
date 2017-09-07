{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Utility functions and types
module Garlic.Util
(
    ratingString,
    durationString,

    ReferencePerson (..),
    defaultReferencePerson,
    NVec (..),
    NutritionLabel (..),
    getNutrition,
)
where

import Control.Lens
import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Garlic.Data.Units
import Garlic.Model
import Garlic.Model.Queries
import Text.Printf
import Database.Persist (entityVal)
import Linear.Vector
import Linear.V2

ratingString :: Int -> String
ratingString r = 
    let x = min 5 (max 0 r)
     in replicate x '★' ++ replicate (5 - x) '☆'
{-# INLINE ratingString #-}

-- | Interprets Duration as milliseconds
durationString :: Int -> String
durationString t = 
    let x :: Double
        x = fromIntegral t / 60000
     in if x >= 120
        then printf "%.1f h" (x / 60)
        else printf "%d min" (truncate x :: Int)
{-# INLINE durationString #-}

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
    { nlServing  :: !Text
    , nlKcal     :: a
    , nlKcalFat  :: a
    , nlFat      :: (NVec a)
    , nlSatFat   :: (NVec a)
    , nlTransFat :: (NVec a)
    , nlCarbs    :: (NVec a)
    , nlFibre    :: (NVec a)
    , nlSugars   :: (NVec a)
    , nlProtein  :: (NVec a)
    }
    deriving (Show, Eq, Functor)

instance Additive NutritionLabel where
    zero = NutritionLabel "" 0 0 zero zero zero zero zero zero zero 
    liftU2 = liftI2
    liftI2 f a b = NutritionLabel
        { nlServing  = nlServing a <> nlServing b
        , nlKcal     = nlKcal a `f` nlKcal b
        , nlKcalFat  = nlKcalFat a `f` nlKcalFat b
        , nlFat      = liftI2 f (nlFat a) (nlFat b)
        , nlSatFat   = liftI2 f (nlSatFat a) (nlSatFat b)
        , nlTransFat = liftI2 f (nlTransFat a) (nlTransFat b)
        , nlCarbs    = liftI2 f (nlCarbs a) (nlCarbs b)
        , nlFibre    = liftI2 f (nlFibre a) (nlFibre b)
        , nlSugars   = liftI2 f (nlSugars a) (nlSugars b)
        , nlProtein  = liftI2 f (nlProtein a) (nlProtein b)
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
