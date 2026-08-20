module Arkham.Enemy.CardDefs.TheDrownedCity.TheDoomOfArkham where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cthulhuAncientEvil :: CardDef
cthulhuAncientEvil =
  unique
    $ (enemy "11701" ("Cthulhu" <:> "Ancient Evil") TheDoomOfArkhamPartII 1)
      { cdFight = fightStar
      , cdEvade = evadeStar
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Massive, Keyword.Patrol CanHaveFloodLevelIncreased]
      }

cthulhuFierceVisage :: CardDef
cthulhuFierceVisage =
  doubleSided "11703b"
    $ (enemy "11703" ("Cthulhu" <:> "Fierce Visage") TheDoomOfArkhamPartII 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdCardTraits = setFromList [Cthulhu, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 5
      , cdUnique = True
      }

cthulhuFierceVisageEnraged :: CardDef
cthulhuFierceVisageEnraged =
  doubleSided "11703"
    $ (enemy "11703b" ("Cthulhu" <:> "Fierce Visage") TheDoomOfArkhamPartII 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdHealth = healthX
      , cdCardTraits = setFromList [Cthulhu, Enraged, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
      , cdVictoryPoints = Just 5
      , cdUnique = True
      }

cthulhuHoaryWings :: CardDef
cthulhuHoaryWings =
  doubleSided "11702b"
    $ (enemy "11702" ("Cthulhu" <:> "Hoary Wings") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdCardTraits = setFromList [Cthulhu, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

cthulhuHoaryWingsEnraged :: CardDef
cthulhuHoaryWingsEnraged =
  doubleSided "11702"
    $ (enemy "11702b" ("Cthulhu" <:> "Hoary Wings") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdHealth = healthX
      , cdCardTraits = setFromList [Cthulhu, Enraged, Elite]
      , cdKeywords = setFromList [Keyword.Alert]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

cthulhuWickedClaw :: CardDef
cthulhuWickedClaw =
  doubleSided "11704b"
    $ (enemy "11704" ("Cthulhu" <:> "Wicked Claw") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdCardTraits = setFromList [Cthulhu, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

cthulhuWickedClawEnraged :: CardDef
cthulhuWickedClawEnraged =
  doubleSided "11704"
    $ (enemy "11704b" ("Cthulhu" <:> "Wicked Claw") TheDoomOfArkhamPartII 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fightX
      , cdEvade = evadeX
      , cdHealth = healthX
      , cdCardTraits = setFromList [Cthulhu, Enraged, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      , cdUnique = True
      }

randallTillinghast :: CardDef
randallTillinghast =
  unique
    $ (enemy "11686" ("Randall Tillinghast" <:> "Out for Your Blood") TheDoomOfArkhamPartI 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Relentless, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }
