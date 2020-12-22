module Test.Main where

import Prelude

import Data.DateTime as DT
import Data.DateTime.ISO8601 (fromISO8601String, toISO8601String)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        readSpec
        printSpec

-- copied over from purescript-datetime-iso

readSpec :: Spec Unit
readSpec = describe "read" do
    it "read standard js ISO strings" do
      "2018-01-09T13:16:43.772Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 772

    it "allow dates without dash" do
      "20180109T13:16:43.772Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 772

    it "allow time without colons" do
      "20180109T131643772Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 772

    it "handles zero milliseconds" do
      "2018-01-09T13:16:43.0Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 0

    it "handles empty milliseconds" do
      "2018-01-09T13:16:43Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 0

    it "handles milliseconds 0-999" do
      "2018-01-09T13:16:43.999Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 999

    it "handles more than 3 digits second fraction" do
      "2018-01-09T13:16:43.1234Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 13 16 43 123

    it "handles milliseconds with one leading zero" do
      "2018-01-09T03:16:43.034Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 3 16 43 34

    it "handles milliseconds with two leading zeros" do
      "2018-01-09T03:16:43.002Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 3 16 43 2

    it "handles two digit milliseconds with leading zero" do
      "2018-01-09T03:16:43.07Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 3 16 43 70

    it "handles two digit milliseconds with leading and trailing zero" do
      "2018-01-09T03:16:43.070Z" `shouldReadAs`
        mkDateTime 2018 DT.January 9 3 16 43 70

    it "should adjust with positive tz offset" do
      "2018-01-09T20:46:43.070+08:15" `shouldReadAs`
        mkDateTime 2018 DT.January 10 5 1 43 70

    it "should adjust with negative tz offset" do
      "2018-01-10T05:01:43.070-08:15" `shouldReadAs`
        mkDateTime 2018 DT.January 9 20 46 43 70

    it "should fail if missing digits" do
      fromISO8601String "2018-1-9T03:16:43.1Z" `shouldSatisfy`
        isNothing

    it"should fail if invalid msec" do
      fromISO8601String "2018-01-09T03:06:03.A70Z" `shouldSatisfy`
        isNothing

    it"should fail if invalid offset" do
      fromISO8601String "2018-01-09T03:06:03.70+A0:00" `shouldSatisfy`
        isNothing

printSpec :: Spec Unit
printSpec = describe "printing" do
  it "prints like an ISO string" do
    mkDateTime 2018 DT.January 9 13 16 43 772 `shouldPrintAs`
      "2018-01-09T13:16:43.772Z"

  it "explicitly prints zero milliseconds" do
    mkDateTime 2018 DT.January 9 13 16 43 0 `shouldPrintAs`
      "2018-01-09T13:16:43.000Z"

  it "prints milliseconds with two leading zeros" do
    mkDateTime 2018 DT.January 9 13 16 43 3 `shouldPrintAs`
      "2018-01-09T13:16:43.003Z"

  it "prints milliseconds with one leading zero" do
    mkDateTime 2018 DT.January 9 13 16 43 40 `shouldPrintAs`
      "2018-01-09T13:16:43.040Z"

shouldReadAs ::
  String ->
  DT.DateTime ->
  Aff Unit
shouldReadAs s exp =
  case fromISO8601String s of
    Just r ->
      r `shouldEqual` exp
    Nothing ->
      fail $ "unable to read datetime - " <> s

shouldPrintAs ::
  DT.DateTime ->
  String ->
  Aff Unit
shouldPrintAs dt exp =
  toISO8601String dt `shouldEqual` exp

-- Helper function for constructing DateTimes.
mkDateTime ::
  Int ->
  DT.Month ->
  Int ->
  Int ->
  Int ->
  Int ->
  Int ->
  DT.DateTime
mkDateTime year month day hh mm ss ms =
  let
    date =
      DT.canonicalDate
        (toEnum' year)
        month
        (toEnum' day)

    time =
      DT.Time
        (toEnum' hh)
        (toEnum' mm)
        (toEnum' ss)
        (toEnum' ms)
  in
    DT.DateTime date time
  where
  toEnum' :: forall a. BoundedEnum a => Int -> a
  toEnum' = toEnum >>> unsafePartial fromJust
