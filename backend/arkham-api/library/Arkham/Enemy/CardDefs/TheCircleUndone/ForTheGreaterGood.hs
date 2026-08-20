module Arkham.Enemy.CardDefs.TheCircleUndone.ForTheGreaterGood where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

cellKeeper :: CardDef
cellKeeper =
  (enemy "05219" "Cell Keeper" ForTheGreaterGood 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Alert
    }

knightOfTheInnerCircle :: CardDef
knightOfTheInnerCircle =
  (enemy "05221" "Knight of the Inner Circle" ForTheGreaterGood 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Aloof, Keyword.Hunter]
    }

knightOfTheOuterVoid :: CardDef
knightOfTheOuterVoid =
  (enemy "05222" "Knight of the Outer Void" ForTheGreaterGood 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Peril, Keyword.Retaliate]
    , cdRevelation = IsRevelation
    }

lodgeJailor :: CardDef
lodgeJailor =
  (enemy "05218" "Lodge Jailor" ForTheGreaterGood 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight]
    , cdKeywords = singleton Keyword.Aloof
    }

nathanWickMasterOfIndoctrination :: CardDef
nathanWickMasterOfIndoctrination =
  unique
    $ doubleSided "05217a"
    $ (enemy "05217b" ("Nathan Wick" <:> "Master of Indoctrination") ForTheGreaterGood 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Alert
      , cdVictoryPoints = Just 1
      }

nathanWickMasterOfInitiation :: CardDef
nathanWickMasterOfInitiation =
  unique
    $ doubleSided "05217b"
    $ (enemy "05217a" ("Nathan Wick" <:> "Master of Initiation") ForTheGreaterGood 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist, SilverTwilight, Elite]
      , cdKeywords = singleton Keyword.Retaliate
      , cdVictoryPoints = Just 1
      }

summonedBeast :: CardDef
summonedBeast =
  (enemy "05220" ("Summoned Beast" <:> "Guardian of the Trap") ForTheGreaterGood 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 6
    , cdCardTraits = setFromList [Monster, SilverTwilight, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate, Keyword.Hunter]
    , cdVictoryPoints = Just 2
    }
