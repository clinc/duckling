-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}

module Duckling.Rules.RO
  ( defaultRules,
    langRules,
    localeRules,
  )
where

import Duckling.Dimensions.Types
import qualified Duckling.AmountOfMoney.RO.Rules as AmountOfMoney
import qualified Duckling.Distance.RO.Rules as Distance
import Duckling.Locale
import qualified Duckling.Duration.RO.Rules as Duration
import qualified Duckling.Numeral.RO.Rules as Numeral
import qualified Duckling.Ordinal.RO.Rules as Ordinal
import qualified Duckling.Quantity.RO.Rules as Quantity
import qualified Duckling.Temperature.RO.Rules as Temperature
import qualified Duckling.Time.RO.Rules as Time
import Duckling.Types
import qualified Duckling.TimeGrain.RO.Rules as TimeGrain
import qualified Duckling.Volume.RO.Rules as Volume

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
langRules (Seal Quantity) = Quantity.rules
langRules (Seal Recurrence) = []
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules RO dim
