module Arkham.Skill.CardDefs.Promo where

import Arkham.Skill.CardDefs.Import

dreamsOfTheDeepTheDeepGate :: CardDef
dreamsOfTheDeepTheDeepGate =
  signature "07005"
    $ (skill "98015" ("Dreams of the Deep" <:> "The Deep Gate") [#wildMinus, #wildMinus] Neutral)
      { cdCardTraits = setFromList [Curse]
      , cdCardSubType = Just Weakness
      , cdOutOfPlayEffects = [InHandEffect]
      }
