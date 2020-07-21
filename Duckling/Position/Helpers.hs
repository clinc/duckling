-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}

module Duckling.Position.Helpers
  ( getIntValue
  , isNatural
  , position
  , isNotMiddle
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Position.Types (PositionData (..), Anchor)
import Duckling.Types
import Duckling.Numeral.Helpers (isNatural)
import Duckling.Numeral.Types (getIntValue)

import qualified Duckling.Position.Types as TPosition

position :: Int -> Int -> Anchor -> PositionData
position i v a = PositionData {TPosition.value = v, TPosition.index = i, TPosition.anchor = a}

isNotMiddle :: Predicate
isNotMiddle (Token Position PositionData {anchor = TPosition.Middle}) = False
isNotMiddle _ = False
