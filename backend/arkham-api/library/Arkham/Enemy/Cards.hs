module Arkham.Enemy.Cards where

import Arkham.Enemy.CardDefEntries (allEnemyCardDefs)
import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.Defs qualified as Homebrew
import Arkham.Keyword qualified as Keyword

{- | An enemy belongs to a player deck exactly when it carries a card subtype
(weakness or basic weakness); everything else is an encounter card.
-}
allPlayerEnemyCards :: Map CardCode CardDef
allPlayerEnemyCards =
  mapFromList
    $ concatMap toCardCodePairs
    $ filter (isJust . cdCardSubType) allEnemyCardDefs

allEncounterEnemyCards :: Map CardCode CardDef
allEncounterEnemyCards =
  (Homebrew.enemiesMap <>)
    $ mapFromList
    $ concatMap toCardCodePairs
    $ filter (isNothing . cdCardSubType) allEnemyCardDefs

allSpecialEnemyCards :: Map CardCode CardDef
allSpecialEnemyCards =
  mapFromList
    $ map
      (toCardCode &&& id)
      [flyingPolyp, reanimatedDead, nyarlathotepTrueShape, golem, extradimensionalEnemy]

flyingPolyp :: CardDef
flyingPolyp =
  (enemy "xpolyp" "Flying Polyp" ShatteredAeons 0)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = singleton Monster
    }

reanimatedDead :: CardDef
reanimatedDead =
  (enemy "xreanimated" "Reanimated Dead" TheWagesOfSin 0)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = singleton Monster
    }

nyarlathotepTrueShape :: CardDef
nyarlathotepTrueShape =
  unique
    $ (enemy "xnyarlathotep" ("Nyarlathotep" <:> "True Shape") WhereTheGodsDwell 0)
      { cdFight = fight 0
      , cdEvade = evade 0
      , cdHealth = health 1
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdVictoryPoints = Just 0
      }

golem :: CardDef
golem =
  (enemy "xgolem" "Golem" WithoutATrace 0)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Outsider]
    , cdKeywords = singleton Keyword.Hunter
    }

extradimensionalEnemy :: CardDef
extradimensionalEnemy =
  (enemy "xextra" "Extradimensional Enemy" FortuneAndFolly 0)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = singleton Extradimensional
    }
