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

import Control.DeepSeq (NFData)
import Data.Aeson (object, KeyValue((.=)), ToJSON(toJSON))
import Data.Hashable (Hashable(hashWithSalt))
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import TextShow (showt)
import Data.Maybe (fromJust)

import Duckling.Resolve (Resolve(..))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)
import Duckling.Time.Types (TimeData(..), TimeValue(..), SingleTimeValue(..), InstantValue(..), start)
import qualified Duckling.TimeGrain.Types as TG
import Control.Monad (join)

data RecurrenceData = RecurrenceData
  { value      :: Int
  , times      :: Int
  , grain      :: Grain
  , anchor     :: Maybe TimeData
  , composite  :: Bool
  , innerGrain :: Maybe Grain
  , innerDay   :: Maybe Int
  , innerInstance  :: Maybe Int
  }
  deriving (Eq, Generic, Ord, Show, NFData)

instance Hashable RecurrenceData where
  hashWithSalt s (RecurrenceData value times grain _ _ _ _ _) = hashWithSalt s
    (0::Int, (value, times, grain))

data RecurrenceValue = RecurrenceValue
  { rValue      :: Int
  , rTimes      :: Int
  , rGrain      :: Grain
  , rAnchor     :: Maybe TimeValue
  , rInnerGrain :: Maybe Grain
  , rInnerDay   :: Maybe Int
  , rInnerInstance   :: Maybe Int
  }
  deriving (Eq, Show)

instance Resolve RecurrenceData where
  type ResolvedValue RecurrenceData = RecurrenceValue
  resolve context options RecurrenceData {value, times, grain, anchor, innerGrain, innerDay, innerInstance} = do
    Just $ case (anchor, innerGrain) of
      (Nothing, Nothing) -> (RecurrenceValue value times grain Nothing Nothing Nothing Nothing, False)
      (Nothing, Just ig) -> (RecurrenceValue value times grain Nothing innerGrain innerDay innerInstance, False)
      (Just d, Nothing) -> do
        (RecurrenceValue value times g a Nothing Nothing Nothing, False)
        where
          a = unwrapValue $ resolve context options d

          -- grain calculation
          g = case a of
            Just tv -> g
              where
                (v, vals) = getVals tv
                g = case vals of
                  (alt1:alt2:_) -> case (getUTC v, getUTC alt1, getUTC alt2) of
                    (Just t1, Just t2, Just t3) -> do
                      let (d1, _) = properFraction $ Time.diffUTCTime t1 t2
                      let (d2, _) = properFraction $ Time.diffUTCTime t1 t3
                      calcGrain $ max (abs d1) (abs d2)
                    _  -> grain
                  (alt:_) -> case (getUTC v, getUTC alt) of
                    (Just t1, Just t2) -> do
                      let (d1, _) = properFraction $ Time.diffUTCTime t1 t2
                      calcGrain d1
                    _  -> grain
                  _ -> grain
            _ -> grain
      (Just d, Just ig) -> do
        (RecurrenceValue value times grain Nothing innerGrain innerDay innerInstance, False)
        where
          a = unwrapValue $ resolve context options d

          -- weekday calculation
          innerDay = case (a, innerGrain) of
            (Just tv, Just TG.Week) -> wd
              where
                -- get day of week from date
                (v, _) = getVals tv
                utc = getUTC v
                wd = case utc of
                  Just t -> do
                    let Time.UTCTime day _ = t
                    let (_, _, dow) = Time.toWeekDate day
                    Just dow
            _ -> Nothing

instance ToJSON RecurrenceValue where
  toJSON RecurrenceValue {rValue, rTimes, rGrain, rAnchor, rInnerGrain, rInnerDay, rInnerInstance} = object
    [ "type"       .= ("value" :: Text)
    , "value"      .= rValue
    , "times"      .= rTimes
    , "unit"       .= rGrain
    , "anchor"     .= rAnchor
    , showt rGrain .= rValue
    , "inner"      .= object
      [ "grain"    .= rInnerGrain
      , "day"      .= rInnerDay
      , "instance" .= rInnerInstance
      ]
    , "normalized" .= object
      [ "unit"  .= ("second" :: Text)
      , "value" .= inSeconds rGrain rValue
      ]
    ]

-- extracts a resolved TimeValue
unwrapValue :: Maybe (TimeValue, Bool) -> Maybe TimeValue
unwrapValue tup = case tup of
  Just (val, _) -> Just val
  _             -> Nothing

-- converts a TimeValue to UTC
getUTC :: SingleTimeValue -> Maybe Time.UTCTime 
getUTC (SimpleValue (InstantValue v g)) = Just $ Time.zonedTimeToUTC v
getUTC _ = Nothing

-- extracts base and alternative TimeValues from a TimeValue
getVals :: TimeValue -> (SingleTimeValue, [SingleTimeValue])
getVals (TimeValue val vals text) = (val, vals)

-- calculates grain from delta between time values
calcGrain :: Int -> TG.Grain
calcGrain delta
  | delta <= 10000 = TG.Year
  | delta <= 100000 = TG.Day
  | delta <= 2000000 = TG.Week
  | delta <= 10000000 = TG.Month
  | otherwise = TG.Year
