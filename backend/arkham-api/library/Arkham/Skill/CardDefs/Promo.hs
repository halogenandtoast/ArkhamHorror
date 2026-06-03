module Arkham.Skill.CardDefs.Promo where

import Arkham.Skill.CardDefs.Import

dreamsOfTheDeepTheDeepGate :: CardDef
dreamsOfTheDeepTheDeepGate =
  (skill "98015" ("Dreams of the Deep" <:> "The Deep Gate") [#wildMinus, #wildMinus] Neutral)
    { cdCardTraits = setFromList [Curse]
    , cdLevel = Nothing
    , cdCardSubType = Just Weakness
    , cdOutOfPlayEffects = [InHandEffect]
    }
