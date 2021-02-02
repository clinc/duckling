-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Recurrence.EN.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.Aeson ( ToJSON(toJSON), Object, Value(Object) )
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Duckling.Recurrence.Types (RecurrenceValue(..))
import Duckling.Resolve (Context)
import Duckling.Testing.Types
    ( examplesCustom,
      testContext,
      testOptions,
      Corpus,
      Datetime,
      Example,
      NegativeCorpus,
      TestPredicate )
import Duckling.Time.Corpus (datetime, datetimeHoliday)
import Duckling.Time.Types (TimeValue(..))
import Duckling.TimeGrain.Types (Grain(..))
import Duckling.Types (ResolvedToken(Resolved, rval), ResolvedVal(RVal))
import qualified Duckling.Recurrence.Types as TRecurrence

check :: ToJSON a => (Context -> a) -> TestPredicate
check f context Resolved{rval = RVal _ v} = case toJSON v of
  Object o -> deleteValues (toJSON (f context)) == deleteValues (Object o)
  _ -> False
  where
    -- need to access and delete values if anchor is set
    deleteValues :: Value -> Value
    deleteValues (Object o) = do
      case unwrapAnchor $ o HashMap.! "anchor" of
        Nothing -> Object o
        Just a -> do
          let anch = Object $ HashMap.delete "values" a
          Object $ HashMap.insert "anchor" anch o
    deleteValues _ = Object HashMap.empty

    unwrapAnchor :: Value -> Maybe Object
    unwrapAnchor (Object x) = Just x
    unwrapAnchor _ = Nothing

examples :: ToJSON a => (Context -> a) -> [Text] -> [Example]
examples f = examplesCustom (check f)

recurrence :: Int -> Int -> Grain -> Context -> RecurrenceValue 
recurrence v t g ctx = RecurrenceValue{TRecurrence.rValue = v, TRecurrence.rTimes = t, TRecurrence.rGrain = g, TRecurrence.rAnchor = Nothing}

anchoredRecurrence :: Int -> Int -> Grain -> Datetime -> Grain -> Context -> RecurrenceValue 
anchoredRecurrence v t g dt dtg ctx = RecurrenceValue{TRecurrence.rValue = v, TRecurrence.rTimes = t, TRecurrence.rGrain = g, TRecurrence.rAnchor = a}
  where
    a = Just $ datetime dt dtg ctx

anchoredRecurrenceHoliday :: Int -> Int -> Grain -> Datetime -> Grain -> Text -> Context -> RecurrenceValue 
anchoredRecurrenceHoliday v t g dt dtg h ctx = RecurrenceValue{TRecurrence.rValue = v, TRecurrence.rTimes = t, TRecurrence.rGrain = g, TRecurrence.rAnchor = a}
  where
    a = Just $ datetimeHoliday dt dtg h ctx

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "for months"
      , "in days"
      , "secretary"
      , "minutes"
      , "I second that"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (recurrence 1 1 Second)
    [ "every second"
    , "per second"
    , "each second"
    ]
  , examples (recurrence 1 1 Minute)
    [ "every minute"
    , "per minute"
    , "every minute"
    ]
  , examples (recurrence 1 1 Hour)
    [ "every hour"
    , "per hour"
    , "every hour"
    , "hourly"
    ]
  , examples (recurrence 1 1 Day)
    [ "every day"
    , "per day"
    , "daily"
    , "once every day"
    ]
  , examples (recurrence 2 1 Day)
    [ "every 2 days"
    , "per 2 days"
    , "bidaily"
    ]
  , examples (recurrence 1 1 Week)
    [ "every week"
    , "per week"
    , "weekly"
    ]
  , examples (recurrence 2 1 Week)
    [ "every 2 weeks"
    , "per 2 week"
    , "biweekly"
    ]
  , examples (recurrence 1 1 Month)
    [ "every month"
    , "per month"
    , "monthly"
    ]
  , examples (recurrence 1 1 Year)
    [ "every year"
    , "per year"
    , "yearly"
    , "annually"
    , "annual"
    ]
  , examples (recurrence 1 1 Decade)
    [ "every decade"
    , "per decade"
    , "each decade"
    ]
  , examples (anchoredRecurrence 1 1 Week (2013, 2, 17, 0, 0, 0) Day)
    [ "every sunday"
    , "per sunday"
    , "each sunday"
    ]
  , examples (anchoredRecurrence 1 1 Year (2013, 6, 2, 0, 0, 0) Day)
    [ "every June 2nd"
    , "per 2nd of June"
    , "each Jun 2"
    ]
  , examples (recurrence 1 3 Day)
    [ "thrice daily"
    , "three times every day"
    , "3 times a day"
    ]
  , examples (recurrence 1 2 Week)
    [ "twice weekly"
    , "two times every week"
    , "2 times a week"
    ]
  , examples (recurrence 1 5 Year)
    [ "five times each year"
    , "five times yearly"
    , "5 times annually"
    ]
  , examples (anchoredRecurrenceHoliday 2 3 Year (2013, 12, 25, 0, 0, 0) Day "Christmas")
    [ "three times every other christmas"
    , "thrice each two xmas"
    , "3 times per alternating christmas day"
    ]
  , examples (recurrence 2 1 Quarter)
    [ "every 2 quarters"
    , "per 2 quarters"
    , "biquarterly"
    , "every two quarters"
    ]
  ]
