-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.Recurrence.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty

import qualified Duckling.Recurrence.EN.Tests as EN

tests :: TestTree
tests = testGroup "Recurrence Tests"
  [ EN.tests
  ]
