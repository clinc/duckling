-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Recurrence.Helpers
  ( recurrence
  , anchoredRecurrence
  , isGrain
  , isNatural
  , recurrentDimension
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Duration.Types (DurationData (DurationData))
import Duckling.Recurrence.Types (RecurrenceData (RecurrenceData))
import Duckling.Time.Types (TimeData(TimeData))
import Duckling.Numeral.Helpers (isNatural)
import Duckling.Types
import qualified Data.Time as Time
import qualified Duckling.Recurrence.Types as TRecurrence
import qualified Duckling.TimeGrain.Types as TG
import qualified Duckling.Duration.Types as TDuration

-- -----------------------------------------------------------------
-- Patterns

isGrain :: TG.Grain -> Predicate
isGrain value (Token TimeGrain grain) = grain == value
isGrain _ _ = False

-- -----------------------------------------------------------------
-- Production

recurrence :: TG.Grain -> Int -> RecurrenceData
recurrence grain n = RecurrenceData {TRecurrence.grain = grain, TRecurrence.value = n, TRecurrence.anchor = Nothing}

anchoredRecurrence :: TG.Grain -> Int -> Maybe Time.UTCTime -> RecurrenceData
anchoredRecurrence grain n t = RecurrenceData {TRecurrence.grain = grain, TRecurrence.value = n, TRecurrence.anchor = t}

recurrentDimension :: Predicate
recurrentDimension (Token Time _) = True
recurrentDimension (Token Duration _) = True
recurrentDimension (Token TimeGrain _) = True
recurrentDimension _ = False
