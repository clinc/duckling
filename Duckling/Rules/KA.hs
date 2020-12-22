-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}

module Duckling.Rules.KA
  ( defaultRules,
    langRules,
    localeRules,
  )
where
import qualified Duckling.AmountOfMoney.KA.Rules as AmountOfMoney

import qualified Duckling.Duration.KA.Rules as Duration
import Duckling.Dimensions.Types
import Duckling.Locale
import qualified Duckling.Ordinal.KA.Rules as Ordinal
import qualified Duckling.Numeral.KA.Rules as Numeral
import qualified Duckling.Time.KA.Rules as Time
import qualified Duckling.TimeGrain.KA.Rules as TimeGrain
import Duckling.Types

defaultRules :: Seal Dimension -> [Rule]
defaultRules = langRules

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = []
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
langRules (Seal (CustomDimension dim)) = dimLangRules KA dim
