module Arkham.Enemy.CardDefs.TheCircleUndone.InTheClutchesOfChaos where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

anetteMasonReincarnatedEvil :: CardDef
anetteMasonReincarnatedEvil =
  doubleSided "05286a"
    $ (enemy "05286b" ("Anette Mason" <:> "Reincarnated Evil") MusicOfTheDamned 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Witch, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

carlSanfordDeathlessFanatic :: CardDef
carlSanfordDeathlessFanatic =
  doubleSided "05288a"
    $ (enemy "05288b" ("Carl Sanford" <:> "Deathless Fanatic") SecretsOfTheUniverse 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, SilverTwilight, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

lodgeEnforcer :: CardDef
lodgeEnforcer =
  (enemy "05309" "Lodge Enforcer" SecretsOfTheUniverse 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Retaliate
    , cdVictoryPoints = Just 1
    }

witnessOfChaos :: CardDef
witnessOfChaos =
  (enemy "05311" "Witness of Chaos" MusicOfTheDamned 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Witch]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }
