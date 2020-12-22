-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Position.EN.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Prelude
import Data.String

import Duckling.Position.Types
import Duckling.Testing.Types
import Duckling.TimeGrain.Types (Grain(..))

import qualified Duckling.Position.Types as TPosition

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (testContext, testOptions, examples)
  where
    examples =
      [ "today"
      , "minutes"
      ]

allExamples :: [Example]
allExamples = concat
  [ examples (PositionData 0 1 TPosition.Start)
             [ "first one from the left side"
             , "one from the top"
             , "first from the top"
             , "the first relative to the start"
             ]
  , examples (PositionData (- 1) 1 TPosition.End)
             [ "last"
             , "bottom one"
             , "the one on the right"
             ]
  , examples (PositionData 1 1 TPosition.Start)
             [ "the second one"
             , "the second from the top"
             , "second relative to the left"
             ]
  ]
