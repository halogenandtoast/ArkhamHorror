module Arkham.Enemy.CardDefs.EdgeOfTheEarth.ToTheForbiddenPeaks where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

constrictingElderThing :: CardDef
constrictingElderThing =
  (enemy "08609" "Constricting Elder Thing" ToTheForbiddenPeaks 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = healthX
    , cdCardTraits = setFromList [Monster, ElderThing]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

terrorOfTheStarsGuardianOfForbiddenPeaks :: CardDef
terrorOfTheStarsGuardianOfForbiddenPeaks =
  (enemy "08608" ("Terror of the Stars" <:> "Guardian of the Forbidden Peaks") ToTheForbiddenPeaks 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fightX
    , cdEvade = evadeX
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Eidolon, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    , cdUnique = True
    }
