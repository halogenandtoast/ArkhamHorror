module Arkham.Enemy.CardDefs.TheScarletKeys.DeadHeat where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

amaranthCorruptionRevealed :: CardDef
amaranthCorruptionRevealed =
  doubleSided "09537a"
    $ (enemy "09537b" ("Amaranth" <:> "Corruption Revealed") DeadHeat 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      , cdUnique = True
      }

amaranthLurkingCorruption :: CardDef
amaranthLurkingCorruption =
  doubleSided "09537b"
    $ (enemy "09537a" ("Amaranth" <:> "Lurking Corruption") DeadHeat 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, Coterie, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      , cdUnique = True
      }

ancientRaider :: CardDef
ancientRaider =
  (enemy "09540" "Ancient Raider" DeadHeat 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Risen]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

khalidBelovedCompanion :: CardDef
khalidBelovedCompanion =
  (enemy "09541" ("Khalid" <:> "Beloved Companion") DeadHeat 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Risen]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }

razinFarhiReanimatedArtificer :: CardDef
razinFarhiReanimatedArtificer =
  (enemy "09538" ("Razin Farhi" <:> "Reanimated Artificer") DeadHeat 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Humanoid, Coterie, Risen, Elite]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    , cdUnique = True
    }

thrall :: CardDef
thrall =
  (enemy "09539" "Thrall" DeadHeat 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Risen]
    }
