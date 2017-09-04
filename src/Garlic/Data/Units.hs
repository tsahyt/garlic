-- | Module defining cooking measurements, conversions, and other unit related
-- utility functions
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Garlic.Data.Units
(
    Unit (..),
    allUnits,
    convert,
    prettyUnit,
    parseUnit
)
where

import Database.Persist.TH
import Data.Tuple
import Data.Foldable
import Data.String
import Text.Printf

import qualified Data.Set as S

-- | Type holding all usable units in Garlic.
data Unit
    -- Volume
    = Tsp
    | Tbsp
    | FlOz
    | Cup
    | Pint
    | Quart
    | Gallon
    | Milliliter
    | Liter
    | Deciliter
    | MetricCup
    -- Mass
    | Pound
    | Ounce
    | Milligram
    | Gram
    | Kilogram
    -- Arbitrary
    | Small
    | Medium
    | Large
    deriving (Show, Eq, Read, Ord, Enum, Bounded)

derivePersistField "Unit"

allUnits :: [Unit]
allUnits = enumFrom (toEnum 0)

instance PrintfArg Unit where
    formatArg x = formatArg (prettyUnit x :: String)
    parseFormat x = parseFormat (prettyUnit x :: String)

conversions :: [(Unit, (Double, Unit))]
conversions = mirror
    [ -- Imperial
      (Tbsp      , (3          , Tsp))
    , (Cup       , (16         , Tbsp))
    , (Cup       , (8          , FlOz))
    , (Pint      , (2          , Cup))
    , (Quart     , (2          , Pint))
    , (Gallon    , (4          , Quart))
    , (FlOz      , (2          , Tbsp))
    , (Pound     , (16         , Ounce))
      -- Metric
    , (Liter     , (1000       , Milliliter))
    , (Deciliter , (10         , Deciliter))
    , (Gram      , (1000       , Milligram))
    , (Kilogram  , (1000       , Gram))
    , (MetricCup , (250        , Milliliter))
      -- Metric/Imperial
    , (Pound     , (453.59237  , Gram))
    , (FlOz      , (0.02957353 , Liter))
      -- Volume to Mass Estimates
    , (FlOz      , (1          , Ounce))
    , (Pint      , (1          , Pound))
    , (Liter     , (1          , Kilogram))
    ]

mirror :: [(Unit, (Double, Unit))] -> [(Unit, (Double, Unit))]
mirror m = m ++ map (\(f, (c,t)) -> (t, (recip c, f))) m

findFactor :: Unit -> Unit -> Maybe Double
findFactor from to = product <$> dfs go (== to) from
    where go u = map (swap . snd) . filter ((== u) . fst) $ conversions

dfs :: Ord a
    => (a -> [(a, b)])
    -> (a -> Bool)
    -> a
    -> Maybe [b]
dfs suc goal = asum . map (dfs' S.empty) . suc
    where dfs' visited (n,x)
              | n `S.member` visited = Nothing
              | goal n               = Just [x]
              | otherwise =
                  let visited' = n `S.insert` visited
                   in (x :) <$> asum [ dfs' visited' s | s <- suc n ]

convert :: Unit -> Double -> Unit -> Maybe Double
convert target amount origin = (amount *) <$> findFactor origin target

prettyUnit :: IsString t => Unit -> t
prettyUnit Tsp        = "tsp"
prettyUnit Tbsp       = "tbsp"
prettyUnit FlOz       = "fl oz"
prettyUnit Cup        = "cup"
prettyUnit Pint       = "pint"
prettyUnit Quart      = "quart"
prettyUnit Gallon     = "gallon"
prettyUnit Milliliter = "ml"
prettyUnit Liter      = "l"
prettyUnit Deciliter  = "dl"
prettyUnit MetricCup  = "metric cup"
prettyUnit Pound      = "lb"
prettyUnit Ounce      = "oz"
prettyUnit Milligram  = "mg"
prettyUnit Gram       = "g"
prettyUnit Kilogram   = "kg"
prettyUnit Small      = "small"
prettyUnit Medium     = "medium"
prettyUnit Large      = "large"

parseUnit :: (Eq t, IsString t) => t -> Unit
parseUnit "tsp"        = Tsp
parseUnit "tbsp"       = Tbsp
parseUnit "fl oz"      = FlOz
parseUnit "cup"        = Cup
parseUnit "pint"       = Pint
parseUnit "quart"      = Quart
parseUnit "gallon"     = Gallon
parseUnit "ml"         = Milliliter
parseUnit "l"          = Liter
parseUnit "dl"         = Deciliter
parseUnit "metric cup" = MetricCup
parseUnit "lb"         = Pound
parseUnit "oz"         = Ounce
parseUnit "mg"         = Milligram
parseUnit "g"          = Gram
parseUnit "kg"         = Kilogram
parseUnit "small"      = Small
parseUnit "medium"     = Medium
parseUnit "large"      = Large
parseUnit _            = error "Invalid Unit!"
