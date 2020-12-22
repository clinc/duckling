-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Position.EN.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Data.Text (Text)
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Position.Types (PositionData(..))
import Duckling.Numeral.Types (NumeralData(..))
import Duckling.Ordinal.Types (OrdinalData(..))
import Duckling.Ordinal.Helpers
import Duckling.Position.Helpers
import Duckling.Regex.Types
import Duckling.Types

import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Position.Types as TPosition

ruleOrdFromStart :: Rule
ruleOrdFromStart = Rule
  { name = "<ordinal> from the start"
  , pattern =
    [ dimension Ordinal
    , regex "(from |relative to |on |in |at |to |of )?(the )?(beginning|start|first|top|left)"
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:_:_) -> 
        Just . Token Position $ position (o - 1) 1 TPosition.Start
      _ -> Nothing
    }

ruleOrdFromEnd :: Rule
ruleOrdFromEnd = Rule
  { name = "<ordinal> from the end"
  , pattern =
    [ dimension Ordinal
    , regex "(from |relative to |on |in |at |to |of )?(the )?(right|bottom|end|last)"
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:_:_) -> 
        Just . Token Position $ position (- o) 1 TPosition.End
      _ -> Nothing
    }

ruleNumFromStart :: Rule
ruleNumFromStart = Rule
  { name = "<integer> from the start"
  , pattern =
    [ Predicate isNatural
    , regex "(from |relative to |on |in |at |to |of )?(the )?(beginning|start|first|top|left)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_:_) -> 
        Just . Token Position $ position 0 (floor v) TPosition.Start
      _ -> Nothing
    }

ruleNumFromEnd :: Rule
ruleNumFromEnd = Rule
  { name = "<integer> from the end"
  , pattern =
    [ Predicate isNatural
    , regex "(from |relative to |on |in |at |to |of )?(the )?(right|bottom|end|last)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_:_) ->
        Just . Token Position $ position (- (floor v)) (floor v) TPosition.End
      _ -> Nothing
    }

ruleOrdNumFromStart :: Rule
ruleOrdNumFromStart = Rule
  { name = "<ordinal> <integer> from the start"
  , pattern =
    [ dimension Ordinal
    , Predicate isNatural
    , regex "(from |relative to |on |in |at |to |of )?(the )?(beginning|start|first|top|left(hand)?)"
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:Token Numeral NumeralData{TNumeral.value = v}:_:_) -> do
        start <- ordMulStart o v
        Just . Token Position $ position start (floor v) TPosition.Start
      _ -> Nothing
    }

ruleOrdNumFromEnd :: Rule
ruleOrdNumFromEnd = Rule
  { name = "<ordinal> <integer> from the end"
  , pattern =
    [ dimension Ordinal
    , Predicate isNatural
    , regex "(from |relative to |on |in |at |to |of )?(the )?(right(hand)?|bottom|end|last)"
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:Token Numeral NumeralData{TNumeral.value = v}:_:_) -> do
        start <- ordMulEnd o v
        Just . Token Position $ position start (floor v) TPosition.End
      _ -> Nothing
    }

ruleStartingNum :: Rule
ruleStartingNum = Rule
  { name = "starting <integer>"
  , pattern =
    [regex "(starting|left(most)?|top(most)?|beginning|highest)"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> 
        Just . Token Position $ position 0 (floor v) TPosition.Start
      _ -> Nothing
    }

ruleEndingNum :: Rule
ruleEndingNum = Rule
  { name = "ending <integer>"
  , pattern =
    [regex "(ending|right(most)?|bottom(most)?|last|lowest|ultimate)"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> do
        Just . Token Position $ position (- (floor v)) (floor v) TPosition.End
      _ -> Nothing
    }

ruleOrdNum :: Rule
ruleOrdNum = Rule
  { name = "<ordinal> <integer>"
  , pattern =
    [ dimension Ordinal
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:Token Numeral NumeralData{TNumeral.value = v}:_) -> do
        start <- ordMulStart o v
        Just . Token Position $ position start (floor v) TPosition.Start
      _ -> Nothing
    }

ruleLast :: Rule
ruleLast = Rule
  { name = "last"
  , pattern =
    [regex "(last|ultimate)"
    ]
  , prod = \tokens -> case tokens of
      (_:_) -> 
        Just . Token Position $ position (- 1) 1 TPosition.End
      _ -> Nothing
    }

ruleNextLast :: Rule
ruleNextLast = Rule
  { name = "next to last"
  , pattern =
    [regex "(next to last|penultimate)"
    ]
  , prod = \tokens -> case tokens of
      (_:_) -> 
        Just . Token Position $ position (- 2) 1 TPosition.End
      _ -> Nothing
    }

ruleOrdLast :: Rule
ruleOrdLast = Rule
  { name = "<ordinal> to last"
  , pattern =
    [ dimension Ordinal
    , regex "to last"
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:_:_) -> 
        Just . Token Position $ position (- o) 1 TPosition.End
      _ -> Nothing
    }

ruleOrdLastNum :: Rule
ruleOrdLastNum = Rule
  { name = "<ordinal> to last <integer>"
  , pattern =
    [ dimension Ordinal
    , regex "to last"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal OrdinalData{TOrdinal.value = o}:_:Token Numeral NumeralData{TNumeral.value = v}:_) -> do
        start <- ordMulEnd o v
        Just . Token Position $ position start (floor v) TPosition.End
      _ -> Nothing
    }

ruleNumInMid :: Rule
ruleNumInMid = Rule
  { name = "<integer> in the middle"
  , pattern =
    [ Predicate isNatural
    , regex "(from |relative to |on |in |at |to |of )?(the )?(mid(dle|point|section)?|center)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:_:_) -> 
        Just . Token Position $ position (- floor ((v - 1) / 2)) (floor v) TPosition.Middle
      _ -> Nothing
    }

ruleMidNum :: Rule
ruleMidNum = Rule
  { name = "middle <integer>"
  , pattern =
    [ regex "(mid(dle|point|section)?|center)"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) -> 
        Just . Token Position $ position (- floor ((v - 1) / 2)) (floor v) TPosition.Middle
      _ -> Nothing
    }

rulePositionOfMid :: Rule
rulePositionOfMid = Rule
  { name = "<position> of middle"
  , pattern =
    [ dimension Position
    , regex "(from |relative to |on |in |at |to |of )?(the )?(mid(dle|point|section)?|center)"
    ]
  , prod = \tokens -> case tokens of
      (Token Position PositionData{TPosition.index = i, TPosition.value = v, TPosition.anchor = a}:_:_)
        -> case a of
         TPosition.Start -> Just . Token Position $ position (- i - v) v TPosition.Middle
         TPosition.End -> Just . Token Position $ position (- i - v + 1) v TPosition.Middle
         _    -> Nothing
      _ -> Nothing
    }

rules :: [Rule]
rules =
  [ ruleOrdFromStart
  , ruleOrdFromEnd
  , ruleNumFromStart
  , ruleNumFromEnd
  , ruleOrdNumFromStart
  , ruleOrdNumFromEnd
  , ruleStartingNum
  , ruleEndingNum
  , ruleOrdNum
  , ruleLast
  , ruleNextLast
  , ruleOrdLast
  , ruleOrdLastNum
  , ruleNumInMid
  , ruleMidNum
  , rulePositionOfMid
  ]
