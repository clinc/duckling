-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Recurrence.Helpers
  ( recurrence
  , timedRecurrence
  , anchoredRecurrence
  , isGrain
  , isNatural
  , recurrentDimension
  , isBasicRecurrence
  , mkComposite
  , dowRecurrence
  , instancedRecurrence
  , tr
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Recurrence.Types (RecurrenceData (..))
import Duckling.Time.Types (TimeData(TimeData))
import Duckling.Numeral.Helpers (isNatural)
import Duckling.Types
import qualified Data.Time as Time
import qualified Duckling.Recurrence.Types as TRecurrence
import qualified Duckling.TimeGrain.Types as TG
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Time.Types as TTime

-- -----------------------------------------------------------------
-- Patterns

isGrain :: TG.Grain -> Predicate
isGrain value (Token TimeGrain grain) = grain == value
isGrain _ _ = False

isBasicRecurrence :: Predicate
isBasicRecurrence (Token Recurrence r) = not $ composite r
isBasicRecurrence _ = False

recurrentDimension :: Predicate
recurrentDimension (Token Time td) = not $ TTime.latent td
recurrentDimension (Token Duration _) = True
recurrentDimension (Token TimeGrain _) = True
recurrentDimension _ = False

-- -----------------------------------------------------------------
-- Production

-- | Convenience helper to return a recurrence token from a rule
tr :: RecurrenceData -> Maybe Token
tr = Just . Token Recurrence

recurrence :: TG.Grain -> Int -> RecurrenceData
recurrence g v = RecurrenceData {TRecurrence.grain = g, TRecurrence.value = v, TRecurrence.anchor = Nothing, TRecurrence.times = 1, TRecurrence.composite = False, TRecurrence.innerGrain = Nothing, TRecurrence.innerDay = Nothing,TRecurrence.innerInstance = Nothing}

timedRecurrence :: TG.Grain -> Int -> Int -> RecurrenceData
timedRecurrence g v t = RecurrenceData {TRecurrence.grain = g, TRecurrence.value = v, TRecurrence.anchor = Nothing, TRecurrence.times = t, TRecurrence.composite = False, TRecurrence.innerGrain = Nothing, TRecurrence.innerDay = Nothing,TRecurrence.innerInstance = Nothing}

anchoredRecurrence :: TG.Grain -> Int -> TimeData -> RecurrenceData
anchoredRecurrence g v a = RecurrenceData {TRecurrence.grain = g, TRecurrence.value = v, TRecurrence.anchor = Just a, TRecurrence.times = 1, TRecurrence.composite = False, TRecurrence.innerGrain = Nothing, TRecurrence.innerDay = Nothing, TRecurrence.innerInstance = Nothing}

mkComposite :: RecurrenceData -> Int -> RecurrenceData
mkComposite RecurrenceData { TRecurrence.grain = g, TRecurrence.value = v, TRecurrence.anchor = a, TRecurrence.times = t, TRecurrence.innerGrain = w, TRecurrence.innerInstance = wn} t' = RecurrenceData {TRecurrence.grain = g, TRecurrence.value = v, TRecurrence.anchor = a, TRecurrence.times = t * t', TRecurrence.composite = True, TRecurrence.innerGrain = w, TRecurrence.innerDay = Nothing, TRecurrence.innerInstance = wn}

dowRecurrence :: Int -> TimeData -> TG.Grain -> TG.Grain -> RecurrenceData
dowRecurrence wn wd g ig = RecurrenceData {TRecurrence.grain = g, TRecurrence.value = 1, TRecurrence.anchor = Just wd, TRecurrence.times = 1, TRecurrence.composite = False, TRecurrence.innerGrain = Just ig, TRecurrence.innerDay = Nothing, TRecurrence.innerInstance = Just wn}

instancedRecurrence :: Maybe Int -> Maybe Int -> TG.Grain -> TG.Grain -> RecurrenceData
instancedRecurrence it is g ig = RecurrenceData {TRecurrence.grain = g, TRecurrence.value = 1, TRecurrence.anchor = Nothing, TRecurrence.times = 1, TRecurrence.composite = False, TRecurrence.innerGrain = Just ig, TRecurrence.innerDay = is, TRecurrence.innerInstance = it}
