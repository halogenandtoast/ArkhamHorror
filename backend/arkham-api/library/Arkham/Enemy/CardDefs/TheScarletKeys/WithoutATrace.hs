module Arkham.Enemy.CardDefs.TheScarletKeys.WithoutATrace where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

mimeticNemesisOtherworldlySubjugator :: CardDef
mimeticNemesisOtherworldlySubjugator =
  (enemy "09690" ("Mimetic Nemesis" <:> "Otherworldly Subjugator") WithoutATrace 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdCardTraits = setFromList [Monster, Outsider, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
    , cdUnique = True
    }

protoplasmicReassembler :: CardDef
protoplasmicReassembler =
  (enemy "09691" "Protoplastmic Reassembler" WithoutATrace 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }
