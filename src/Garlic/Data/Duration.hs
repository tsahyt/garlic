{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
module Garlic.Data.Duration
(
    ratingString,
    durationString,
    parseDuration,
)
where

import Data.Monoid
import Data.Text (Text, pack)
import Data.Attoparsec.Text

ratingString :: Int -> String
ratingString r = 
    let x = min 5 (max 0 r)
     in replicate x '★' ++ replicate (5 - x) '☆'
{-# INLINE ratingString #-}

msPerSecond, msPerMinute, msPerHour, msPerDay :: Num a => a
msPerSecond = 1000
msPerMinute = 60000
msPerHour   = 3600000
msPerDay    = 86400000

-- | Interprets Duration as milliseconds
durationString :: Int -> Text
durationString t
    | t >= msPerDay    = fst (extract msPerDay) <> "d " 
                      <> durationString (snd (extract msPerDay))
    | t >= msPerHour   = fst (extract msPerHour) <> "h " 
                      <> durationString (snd (extract msPerHour))
    | t >= msPerMinute = fst (extract msPerMinute) <> "min " 
                      <> durationString (snd (extract msPerMinute))
    | t >= msPerSecond = fst (extract msPerSecond) <> "s "
                      <> durationString (snd (extract msPerSecond))
    | otherwise     = ""
    where extract x =
              let y = last [ z | z <- [1..t `div` x], t >= z * x ]
               in (pack . show $ y, t - y * x)
{-# INLINE durationString #-}

parseDuration :: Text -> Maybe Int
parseDuration = either (const Nothing) Just . parseOnly duration
{-# INLINE parseDuration #-}

duration :: Parser Int
duration = truncate . sum <$> many1 (comp <* skipSpace)
    where comp = choice [ (* msPerSecond) <$> sec, (* msPerMinute) <$> mins
                        , (* msPerHour) <$> hour, (* msPerDay) <$> day ]

          dec  = double <* skipSpace

          sec  = dec <* choice [ string "seconds", string "second"
                               , string "sec", string "s" ]
          mins = dec <* choice [ string "minutes", string "minute"
                               , string "min", string "m" ]
          hour = dec <* choice [ string "hours", string "hour", string "h" ]
          day  = dec <* choice [ string "days", string "day", string "d" ]
