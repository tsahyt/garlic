-- | Utility functions
module Garlic.Util
(
    ratingString,
    durationString,
)
where

import Text.Printf

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
