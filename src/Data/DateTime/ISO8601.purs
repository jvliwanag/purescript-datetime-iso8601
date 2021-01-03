module Data.DateTime.ISO8601
       ( fromISO8601String
       , toISO8601String
       ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.DateTime (DateTime(..), Time(..), exactDate)
import Data.DateTime as DateTime
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foldable (fold)
import Data.Int (pow)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Hours(..), Milliseconds(..), Minutes(..), fromDuration, negateDuration)

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

  year <- requireNdxEnum m 1
  month <- requireNdxEnum m 2
  day <- requireNdxEnum m 3

  date <- exactDate year month day

  hour <- requireNdxEnum m 4
  min <- requireNdxEnum m 5
  sec <- requireNdxEnum m 6

  msecMatch <- m !! 7
  msec <- case msecMatch of
    Nothing -> pure bottom
    Just msecStr -> readMsecStr msecStr

  let time = Time hour min sec msec
      baseDateTime = DateTime date time
      offset = fold $ readOffset m

  DateTime.adjust (negateDuration offset) baseDateTime

  where
    requireNdxEnum :: forall a. BoundedEnum a => NonEmptyArray (Maybe String) -> Int -> Maybe a
    requireNdxEnum m ndx = requireNdxInt m ndx >>= toEnum

    requireNdxInt :: NonEmptyArray (Maybe String) -> Int -> Maybe Int
    requireNdxInt m ndx = requireNdx m ndx >>= Int.fromString

    requireNdx :: NonEmptyArray (Maybe String) -> Int -> Maybe String
    requireNdx m ndx = join (m !! ndx)

    readMsecStr msecStr = do
      base <- Int.fromString msecStr
      toEnum $ base * pow 10 (3 - String.length msecStr)

    offsetSign :: String -> Milliseconds -> Milliseconds
    offsetSign "-" (Milliseconds d) = Milliseconds (-d)
    offsetSign _ d = d

    readOffset m = ado
      offsetFn <- offsetSign <$> requireNdx m 8
      offsetHrs <- fromDuration <<< Hours <<< Int.toNumber <$> requireNdxInt m 9
      offsetMins <- fromDuration <<< Minutes <<< Int.toNumber <$> requireNdxInt m 10
      in offsetFn $ offsetHrs <> offsetMins

iso8601Regex :: Regex
iso8601Regex =
  unsafeRegex """^(\d{4})-?(\d{2})-?(\d{2})T(\d{2}):?(\d{2}):?(\d{2})(?:\.?(\d{1,3})\d*)?(?:Z|(\+|-)(\d{2}):?(\d{2}))$""" noFlags

toISO8601String :: DateTime -> String
toISO8601String (DateTime date time) =
  zeroPadEnum 4 (DateTime.year date)
  <> "-"
  <> zeroPadEnum 2 (DateTime.month date)
  <> "-"
  <> zeroPadEnum 2 (DateTime.day date)
  <> "T"
  <> zeroPadEnum 2 (DateTime.hour time)
  <> ":"
  <> zeroPadEnum 2 (DateTime.minute time)
  <> ":"
  <> zeroPadEnum 2 (DateTime.second time)
  <> "."
  <> zeroPadEnum 3 (DateTime.millisecond time)
  <> "Z"

zeroPadEnum :: forall a. BoundedEnum a => Int -> a -> String
zeroPadEnum n a = pad' (n - String.length i') i'
  where
    i' = show $ fromEnum a

    pad' n' acc | n' <= 0 = acc
                | otherwise = pad' (n' - 1) ("0" <> acc)
