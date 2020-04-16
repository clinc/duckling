-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Recurrence.EN.Rules
  ( rules
  ) where

import Data.Semigroup ((<>))
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Recurrence.Helpers
import Duckling.Duration.Types (DurationData (..))
import Duckling.Recurrence.Types (RecurrenceData(..))
import Duckling.Numeral.Helpers (parseInt, parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Time.Types (TimeData(..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Time.Types as TTime
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

-- TODO: set anchor and grain from recurrent value
ruleRecurrenceEvery :: Rule
ruleRecurrenceEvery = Rule
  { name = "every|per|each <integer> <time/duration/unit-of-time>"
  , pattern =
    [ regex "every|per|each"
    , Predicate recurrentDimension
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.timeGrain}:_) ->
        Just . Token Recurrence $ recurrence timeGrain 1
      (_:Token Duration DurationData{TDuration.grain, TDuration.value}:_) ->
        Just . Token Recurrence $ recurrence grain value
      (_:Token TimeGrain grain:_) ->
        Just . Token Recurrence $ recurrence grain 1
      _ -> Nothing
  }

ruleRecurrenceEveryNumeral :: Rule
ruleRecurrenceEveryNumeral = Rule
  { name = "every|per|each <time/duration/unit-of-time>"
  , pattern =
    [ regex "every|per|each"
    , Predicate isNatural
    , Predicate recurrentDimension
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value}:Token Time TimeData{TTime.timeGrain}:_) ->
        Just . Token Recurrence $ recurrence timeGrain $ floor value
      (_:Token Numeral NumeralData{TNumeral.value = v}:Token Duration DurationData{TDuration.grain, TDuration.value}:_) ->
        Just . Token Recurrence $ recurrence grain $ floor v
      (_:Token Numeral NumeralData{TNumeral.value}:Token TimeGrain grain:_) ->
        Just . Token Recurrence $ recurrence grain $ floor value
      _ -> Nothing
  }

ruleRecurrenceLy :: Rule
ruleRecurrenceLy = Rule
  { name = "hourly|daily|weekly|monthly|yearly"
  , pattern =
    [regex "((bi)?(hour|dai|week|month|quarter|year|annual)ly)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "hourly"      -> Just . Token Recurrence $ recurrence TG.Hour 1
         "bihourly"    -> Just . Token Recurrence $ recurrence TG.Hour 2
         "daily"       -> Just . Token Recurrence $ recurrence TG.Day 1
         "bidaily"     -> Just . Token Recurrence $ recurrence TG.Day 2
         "weekly"      -> Just . Token Recurrence $ recurrence TG.Week 1
         "biweekly"    -> Just . Token Recurrence $ recurrence TG.Week 2
         "monthly"     -> Just . Token Recurrence $ recurrence TG.Month 1
         "bimonthly"   -> Just . Token Recurrence $ recurrence TG.Month 2
         "quarterly"   -> Just . Token Recurrence $ recurrence TG.Quarter 1
         "biquarterly" -> Just . Token Recurrence $ recurrence TG.Quarter 2
         "yearly"      -> Just . Token Recurrence $ recurrence TG.Year 1
         "biyearly"    -> Just . Token Recurrence $ recurrence TG.Year 2
         "annually"    -> Just . Token Recurrence $ recurrence TG.Year 1
         "biannually"  -> Just . Token Recurrence $ recurrence TG.Year 2
         _             -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleRecurrenceEvery
  , ruleRecurrenceEveryNumeral
  , ruleRecurrenceLy
  ]
