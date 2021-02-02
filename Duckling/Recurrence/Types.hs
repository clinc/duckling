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
import Data.Tuple.Extra (both)
import GHC.Generics (Generic)
import TextShow (showt)
import Data.Maybe (fromJust)

import Duckling.Resolve (Resolve(..))
import Duckling.TimeGrain.Types (Grain(..), inSeconds)
import Duckling.Time.Types (TimeData(..), TimeValue(..), SingleTimeValue(..), InstantValue(..))
import qualified Duckling.TimeGrain.Types as TG
import Control.Monad (join)

data RecurrenceData = RecurrenceData
  { value     :: Int
  , times     :: Int
  , grain     :: Grain
  , anchor    :: Maybe TimeData
  , composite :: Bool
  }
  deriving (Eq, Generic, Ord, Show, NFData)

instance Hashable RecurrenceData where
  hashWithSalt s (RecurrenceData value times grain _ _) = hashWithSalt s
    (0::Int, (value, times, grain))

data RecurrenceValue = RecurrenceValue
  { rValue  :: Int
  , rTimes  :: Int
  , rGrain  :: Grain
  , rAnchor :: Maybe TimeValue
  }
  deriving (Eq, Show)

instance Resolve RecurrenceData where
  type ResolvedValue RecurrenceData = RecurrenceValue
  resolve context options RecurrenceData {value, times, grain, anchor} = do
    Just $ case anchor of
      Nothing -> (RecurrenceValue value times grain Nothing, False)
      Just d -> do
        (RecurrenceValue value times g a, False)
        where
          a = unwrapValue $ resolve context options d
          g = case a of
            Just tv -> g
              where
                -- determine grain from distance between values
                (v, vals) = getVals tv
                g = case vals of
                  (alt:_) -> g
                    where
                      g = case (getUTC v, getUTC alt) of
                        (Just t1, Just t2) -> do
                          let (d, _) = properFraction $ Time.diffUTCTime t1 t2
                          let delta = abs d
                          if      delta <= 10000 then
                            TG.Year
                          else if delta <= 100000 then
                            TG.Day
                          else if delta <= 1000000 then
                            TG.Week
                          else if delta <= 10000000 then
                            TG.Month
                          else
                            TG.Year
                        _  -> grain
                      getUTC :: SingleTimeValue -> Maybe Time.UTCTime 
                      getUTC (SimpleValue (InstantValue v g)) = Just $ Time.zonedTimeToUTC v
                      getUTC _ = Nothing
                  [] -> grain
                getVals :: TimeValue -> (SingleTimeValue, [SingleTimeValue])
                getVals (TimeValue val vals text) = (val, vals)
            Nothing -> grain
          unwrapValue :: Maybe (TimeValue, Bool) -> Maybe TimeValue
          unwrapValue tup = case tup of
            Just (val, _) -> Just val
            Nothing       -> Nothing

instance ToJSON RecurrenceValue where
  toJSON RecurrenceValue {rValue, rTimes, rGrain, rAnchor} = object
    [ "type"       .= ("value" :: Text)
    , "value"      .= rValue
    , "times"      .= rTimes
    , "unit"       .= rGrain
    , "anchor"     .= rAnchor
    , showt rGrain  .= rValue
    , "normalized" .= object
      [ "unit"  .= ("second" :: Text)
      , "value" .= inSeconds rGrain rValue
      ]
    ]
