module Arkham.Enemy.CardDefs.TheCircleUndone.TheSecretName where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

brownJenkin :: CardDef
brownJenkin =
  unique
    $ (enemy "05148" ("Brown Jenkin" <:> "The Witch's Familiar") TheSecretName 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 4
      , cdHealth = health 1
      , cdCardTraits = setFromList [Creature, Familiar, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      }

nahab :: CardDef
nahab =
  unique
    $ (enemy "05149" ("Nahab" <:> "She Who Signed the Black Book") TheSecretName 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Monster, Geist, Witch, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      }
