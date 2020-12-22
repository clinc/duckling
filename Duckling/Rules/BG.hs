-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}

module Duckling.Rules.BG
  ( defaultRules,
    langRules,
    localeRules,
  )
where

import Duckling.Dimensions.Types
import qualified Duckling.AmountOfMoney.BG.Rules as AmountOfMoney
import qualified Duckling.Distance.BG.Rules as Distance
import Duckling.Locale
import qualified Duckling.Numeral.BG.Rules as Numeral
import qualified Duckling.Ordinal.BG.Rules as Ordinal
import qualified Duckling.Duration.BG.Rules as Duration
import qualified Duckling.Time.BG.Rules as Time
import qualified Duckling.TimeGrain.BG.Rules as TimeGrain
import Duckling.Types

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = []
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Position) = []
langRules (Seal Quantity) = []
langRules (Seal Recurrence) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = []
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = []
langRules (Seal (CustomDimension dim)) = dimLangRules BG dim
