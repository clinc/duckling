-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}

module Duckling.Rules.SV
  ( defaultRules,
    langRules,
    localeRules,
  )
where

import qualified Duckling.AmountOfMoney.SV.Rules as AmountOfMoney
import Duckling.Dimensions.Types
import qualified Duckling.Distance.SV.Rules as Distance
import qualified Duckling.Duration.SV.Rules as Duration
import Duckling.Locale
import qualified Duckling.Ordinal.SV.Rules as Ordinal
import qualified Duckling.Numeral.SV.Rules as Numeral
import qualified Duckling.Time.SV.Rules as Time
import Duckling.Types
import qualified Duckling.TimeGrain.SV.Rules as TimeGrain

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
langRules (Seal (CustomDimension dim)) = dimLangRules SV dim
