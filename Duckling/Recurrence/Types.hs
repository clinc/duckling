-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Duckling.Recurrence.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Semigroup
import Data.Text (Text)
import Data.Tuple.Extra (both)
import GHC.Generics
import TextShow (showt)
import Prelude

import Duckling.Resolve (Resolve(..))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)
import qualified Data.Time as Time

data RecurrenceData = RecurrenceData
  { value  :: Int
  , grain  :: Grain
  , anchor :: Maybe Time.UTCTime
  }
  deriving (Eq, Generic, Ord, Show, NFData)

instance Hashable RecurrenceData where
  hashWithSalt s (RecurrenceData value grain _) = hashWithSalt s
    (0::Int, (value, grain))

instance Resolve RecurrenceData where
  type ResolvedValue RecurrenceData = RecurrenceData
  resolve _ _ x = Just (x, False)

instance Semigroup RecurrenceData where
  d1@(RecurrenceData _ g1 t) <> d2@(RecurrenceData _ g2 _) = RecurrenceData (v1+v2) g t
    where
    g = g1 `min` g2
    (RecurrenceData v1 _ t, RecurrenceData v2 _ _) = both (withGrain g) (d1,d2)

instance ToJSON RecurrenceData where
  toJSON RecurrenceData {value, grain, anchor} = object
    [ "type"       .= ("value" :: Text)
    , "value"      .= value
    , "unit"       .= grain
    , "anchor"     .= anchor
    , showt grain  .= value
    , "normalized" .= object
      [ "unit"  .= ("second" :: Text)
      , "value" .= inSeconds grain value
      ]
    ]

-- | Convert a duration to the given grain, rounded to the
-- nearest integer. For example, 1 month is converted to 4 weeks.
withGrain :: Grain -> RecurrenceData -> RecurrenceData
withGrain g d@(RecurrenceData v1 g1 t)
  | g == g1 = d
  | otherwise = RecurrenceData v g t
      where
      v = round $ inSeconds g1 (fromIntegral v1 :: Double) / inSeconds g 1
