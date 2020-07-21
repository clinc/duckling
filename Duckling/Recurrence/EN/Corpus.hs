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

import Prelude
import Data.String

import Duckling.Recurrence.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

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
  [ examples (RecurrenceData 1 Second Nothing)
             [ "every second"
             , "per second"
             , "each second"
             ]
  , examples (RecurrenceData 1 Minute Nothing)
             [ "every minute"
             , "per minute"
             , "every minute"
             ]
  , examples (RecurrenceData 1 Hour Nothing)
             [ "every hour"
             , "per hour"
             , "every hour"
             ]
  , examples (RecurrenceData 1 Day Nothing)
             [ "every day"
             , "per day"
             , "daily"
             ]
  , examples (RecurrenceData 2 Day Nothing)
             [ "every 2 days"
             , "per 2 days"
             ]
  , examples (RecurrenceData 1 Week Nothing)
             [ "every week"
             , "per week"
             , "weekly"
             ]
  , examples (RecurrenceData 2 Week Nothing)
             [ "every 2 weeks"
             , "per 2 week"
             , "biweekly"
             ]
  , examples (RecurrenceData 1 Month Nothing)
             [ "every month"
             , "per month"
             , "monthly"
             ]
  , examples (RecurrenceData 1 Year Nothing)
             [ "every year"
             , "per year"
             , "yearly"
             ]
  , examples (RecurrenceData 1 Decade Nothing)
             [ "every decade"
             , "per decade"
             , "each decade"
             ]
  ]
