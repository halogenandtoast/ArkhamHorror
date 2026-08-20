module Arkham.Enemy.CardDefs.BrethrenOfAsh.SpreadingFlames where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

bystander :: CardDef
bystander =
  (enemy "12123" "Bystander" Bystanders 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Civilian]
    , cdKeywords = setFromList [Keyword.Doomed]
    }

mutatedExperiment :: CardDef
mutatedExperiment =
  (enemy "12132" "Mutated Experiment" MadScience 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Creature, Mutated]
    }

servantOfFlameRagingFury :: CardDef
servantOfFlameRagingFury =
  unique
    $ (enemy "12114" ("Servant of Flame" <:> "Raging Fury") SpreadingFlames 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }
