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

module Duckling.Position.Types where

import Control.DeepSeq
import Data.Aeson
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Prelude

import Duckling.Resolve (Resolve(..))

data Anchor
  = Start
  | End
  | Middle
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance ToJSON Anchor where
  toJSON Start    = "start"
  toJSON End  = "end"
  toJSON Middle   = "middle"

data PositionData = PositionData
  { value :: Int
  , index :: Int
  , anchor :: Anchor
  }
  deriving (Eq, Generic, Hashable, Show, Ord, NFData)

instance Resolve PositionData where
  type ResolvedValue PositionData = PositionData
  resolve _ _ x = Just (x, False)

instance ToJSON PositionData where
  toJSON PositionData {value, index, anchor} = object
    [ "type" .= ("value" :: Text)
    , "span" .= value
    , "offset" .= index
    , "anchor" .= anchor
    ]
