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

import qualified Data.Text as Text

import Duckling.Dimensions.Types
    ( Dimension(RegexMatch, Ordinal, Numeral, TimeGrain, Duration,
                Recurrence, Time) )
import Duckling.Recurrence.Helpers
    ( isNatural,
      anchoredRecurrence,
      isBasicRecurrence,
      mkComposite,
      recurrence,
      recurrentDimension,
      timedRecurrence,
      tr )
import Duckling.Duration.Types (DurationData (..))
import Duckling.Recurrence.Types (RecurrenceData(..))
import Duckling.Numeral.Helpers (parseInt, parseInteger)
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Time.Helpers (getIntValue)
import Duckling.Time.Types (TimeData(..))
import Duckling.Regex.Types ( GroupMatch(GroupMatch) )
import Duckling.Types
    ( dimension,
      regex,
      PatternItem(Predicate),
      Rule(..),
      Token(Token) )
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.TimeGrain.Types as TG

ruleEvery :: Rule
ruleEvery = Rule
  { name = "every|per|each <time/duration/unit-of-time>"
  , pattern =
    [ regex "every|per|each"
    , Predicate recurrentDimension
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) ->
        tr $ anchoredRecurrence (timeGrain td) 1 td
      (_:Token Duration DurationData{TDuration.grain, TDuration.value}:_) ->
        tr $ recurrence grain value
      (_:Token TimeGrain grain:_) ->
        tr $ recurrence grain 1
      _ -> Nothing
  }

ruleEveryNumeral :: Rule
ruleEveryNumeral = Rule
  { name = "every|per|each <integer> <time/duration/unit-of-time>"
  , pattern =
    [ regex "every|per|each"
    , Predicate isNatural
    , Predicate recurrentDimension
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value}:Token Time td:_) ->
        tr $ anchoredRecurrence (timeGrain td) (floor value) td
      (_:Token Numeral NumeralData{TNumeral.value}:Token Duration DurationData{TDuration.grain}:_) ->
        tr $ recurrence grain $ floor value
      (_:Token Numeral NumeralData{TNumeral.value}:Token TimeGrain grain:_) ->
        tr $ recurrence grain $ floor value
      _ -> Nothing
  }

ruleEveryOther :: Rule
ruleEveryOther = Rule
  { name = "every|per|each other|alternating <time/duration/unit-of-time>"
  , pattern =
    [ regex "every|per|each"
    , regex "other|alternating"
    , Predicate recurrentDimension
    ]
  , prod = \tokens -> case tokens of
      (_:_:Token Time td:_) ->
        tr $ anchoredRecurrence (timeGrain td) 2 td
      (_:_:Token Duration DurationData{TDuration.grain, TDuration.value}:_) ->
        tr $ recurrence grain 2
      (_:_:Token TimeGrain grain:_) ->
        tr $ recurrence grain 2
      _ -> Nothing
  }

ruleSemiLy :: Rule
ruleSemiLy = Rule
  { name = "(bi|semi)(hourly|daily|weekly|monthly|yearly)"
  , pattern =
    [regex "(bi|semi)[\\s\\-]*(hour|dai|week|month|quarter|year|annual)ly"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (lead:grain:_)):
       _) -> do
          (v, t) <- case Text.toLower lead of
            "bi"   -> Just (2, 1)
            "semi" -> Just (1, 2)
            _      -> Just (1, 1)
          g <- case Text.toLower grain of
            "hour"    -> Just TG.Hour
            "dai"     -> Just TG.Day
            "week"    -> Just TG.Week
            "month"   -> Just TG.Month
            "quarter" -> Just TG.Quarter
            "year"    -> Just TG.Year
            "annual"  -> Just TG.Year 
            _         -> Nothing
          tr $ timedRecurrence g v t
      _ -> Nothing
  }

ruleLy :: Rule
ruleLy = Rule
  { name = "hourly|daily|weekly|monthly|yearly"
  , pattern =
    [regex "(hour|dai|week|month|quarter|year|annual)ly"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (grain:_)):
       _) -> do
          g <- case Text.toLower grain of
            "hour"    -> Just TG.Hour
            "dai"     -> Just TG.Day
            "week"    -> Just TG.Week
            "month"   -> Just TG.Month
            "quarter" -> Just TG.Quarter
            "year"    -> Just TG.Year
            "annual"  -> Just TG.Year 
            _         -> Nothing
          tr $ timedRecurrence g 1 1
      _ -> Nothing
  }

ruleSemiAnnual :: Rule
ruleSemiAnnual = Rule
  { name = "semi-annual"
  , pattern =
    [regex "(bi|semi)[\\s\\-]*annual"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       _) -> do
          (v, t) <- case Text.toLower match of
            "bi"   -> Just (2, 1)
            "semi" -> Just (1, 2)
            _      -> Just (1, 1)
          tr $ timedRecurrence TG.Year v t
      _ -> Nothing
  }

ruleAnnual :: Rule
ruleAnnual = Rule
  { name = "annual"
  , pattern =
    [regex "annual"
    ]
  , prod = \_ -> tr $ recurrence TG.Year 1
  }

ruleEveryday :: Rule
ruleEveryday = Rule
  { name = "everyday"
  , pattern =
    [regex "everyday"
    ]
  , prod = \_ -> tr $ recurrence TG.Day 1
  }

ruleCompositeTimes :: Rule
ruleCompositeTimes = Rule
  { name = "<integer> times <recurrence>"
  , pattern =
    [ Predicate isNatural
    , regex "times?"
    , Predicate isBasicRecurrence
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value}:_:Token Recurrence r:_) -> 
        tr $ mkComposite r (floor value)
      _ -> Nothing
  }

ruleCompositeOnce :: Rule
ruleCompositeOnce = Rule
  { name = "once|twice|thrice <recurrence>"
  , pattern =
    [ regex "(once|twice|thrice)"
    , Predicate isBasicRecurrence
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Recurrence r:_) ->
        case Text.toLower match of
          "once"   -> tr $ mkComposite r 1
          "twice"  -> tr $ mkComposite r 2
          "thrice" -> tr $ mkComposite r 3
          _        -> Nothing
      _ -> Nothing
  }

ruleCompositeTimesAGrain :: Rule
ruleCompositeTimesAGrain = Rule
  { name = "<integer> times a <grain>"
  , pattern =
    [ Predicate isNatural
    , regex "times? a"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value}:_:Token TimeGrain grain:_) -> 
        tr $ mkComposite (recurrence grain 1) (floor value)
      _ -> Nothing
  }

ruleCompositeOnceAGrain :: Rule
ruleCompositeOnceAGrain = Rule
  { name = "once|twice|thrice a <grain>"
  , pattern =
    [ regex "(once|twice|thrice) a"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:
       _) -> case Text.toLower match of
          "once"   -> tr $ mkComposite (recurrence grain 1) 1
          "twice"  -> tr $ mkComposite (recurrence grain 1) 2
          "thrice" -> tr $ mkComposite (recurrence grain 1) 3
          _        -> Nothing
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleEvery
  , ruleEveryNumeral
  , ruleEveryOther
  , ruleSemiLy
  , ruleLy
  , ruleSemiAnnual
  , ruleAnnual
  , ruleEveryday
  , ruleCompositeTimes
  , ruleCompositeOnce
  , ruleCompositeTimesAGrain
  , ruleCompositeOnceAGrain
  ]
