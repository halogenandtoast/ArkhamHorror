module Arkham.Enemy.CardDefs.TheForgottenAge.ThreadsOfFate where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

harlanEarnstoneCrazedByTheCurse :: CardDef
harlanEarnstoneCrazedByTheCurse =
  unique
    $ doubleSided "04122"
    $ (enemy "04122b" ("Harlan Earnstone" <:> "Crazed by the Curse") ThreadsOfFate 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Cursed, Elite]
      , cdVictoryPoints = Just 1
      }

henryDeveauAlejandrosKidnapper :: CardDef
henryDeveauAlejandrosKidnapper =
  unique
    $ doubleSided "04130"
    $ (enemy "04130b" ("Henry Deveau" <:> "Alejandro's Kidnapper") ThreadsOfFate 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
      , cdVictoryPoints = Just 1
      , cdKeywords = singleton Keyword.Retaliate
      }

mariaDeSilvaKnowsMoreThanSheLetsOn :: CardDef
mariaDeSilvaKnowsMoreThanSheLetsOn =
  unique
    $ doubleSided "04137"
    $ (enemy "04137b" ("Maria DeSilva" <:> "Knows More Than She Lets On") ThreadsOfFate 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Conspirator, Elite]
      , cdVictoryPoints = Just 1
      , cdKeywords = singleton Keyword.Retaliate
      }
