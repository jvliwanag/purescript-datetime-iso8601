module Data.DateTime.ISO8601
       ( fromISO8601String
       , toISO8601String
       ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.DateTime (DateTime(..), Time(..), exactDate)
import Data.DateTime as DT
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int (pow)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

-- See https://www.w3.org/TR/NOTE-datetime

{-
  Complete date plus hours and minutes:
    YYYY-MM-DDThh:mmTZD (eg 1997-07-16T19:20+01:00)
  Complete date plus hours, minutes and seconds:
    YYYY-MM-DDThh:mm:ssTZD (eg 1997-07-16T19:20:30+01:00)
  Complete date plus hours, minutes, seconds and a decimal fraction of a
second
    YYYY-MM-DDThh:mm:ss.sTZD (eg 1997-07-16T19:20:30.45+01:00)

where:

     YYYY = four-digit year
     MM   = two-digit month (01=January, etc.)
     DD   = two-digit day of month (01 through 31)
     hh   = two digits of hour (00 through 23) (am/pm NOT allowed)
     mm   = two digits of minute (00 through 59)
     ss   = two digits of second (00 through 59)
     s    = one or more digits representing a decimal fraction of a second
     TZD  = time zone designator (Z or +hh:mm or -hh:mm)
-}

fromISO8601String :: String -> Maybe DateTime
fromISO8601String s = do
  m <- Regex.match iso8601Regex s

  year <- readMatchNdx m 1
  month <- readMatchNdx m 2
  day <- readMatchNdx m 3

  date <- exactDate year month day

  hour <- readMatchNdx m 4
  min <- readMatchNdx m 5
  sec <- readMatchNdx m 6

  msecMatch <- m !! 7
  msec <- case msecMatch of
    Nothing -> pure bottom
    Just msecStr -> readMsecStr msecStr

  let time = Time hour min sec msec

  pure (DateTime date time)

  where
    readMatchNdx :: forall a. BoundedEnum a => NonEmptyArray (Maybe String) -> Int -> Maybe a
    readMatchNdx m ndx = join (m !! ndx) >>= Int.fromString >>= toEnum

    readMsecStr msecStr = do
      base <- Int.fromString msecStr
      toEnum $ base * pow 10 (3 - String.length msecStr)

iso8601Regex :: Regex
iso8601Regex =
  unsafeRegex """^(\d{4})-?(\d{2})-?(\d{2})T(\d{2}):?(\d{2}):?(\d{2})(?:\.?(\d{1,3})\d*)?Z$""" noFlags

toISO8601String :: DateTime -> String
toISO8601String (DateTime date time) =
  zeroPadEnum 4 (DT.year date)
  <> "-"
  <> zeroPadEnum 2 (DT.month date)
  <> "-"
  <> zeroPadEnum 2 (DT.day date)
  <> "T"
  <> zeroPadEnum 2 (DT.hour time)
  <> ":"
  <> zeroPadEnum 2 (DT.minute time)
  <> ":"
  <> zeroPadEnum 2 (DT.second time)
  <> "."
  <> zeroPadEnum 3 (DT.millisecond time)
  <> "Z"

zeroPadEnum :: forall a. BoundedEnum a => Int -> a -> String
zeroPadEnum n a = pad' (n - String.length i') i'
  where
    i' = show $ fromEnum a

    pad' n' acc | n' <= 0 = acc
                | otherwise = pad' (n' - 1) ("0" <> acc)
