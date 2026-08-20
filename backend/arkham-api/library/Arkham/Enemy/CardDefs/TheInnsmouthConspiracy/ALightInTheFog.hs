module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.ALightInTheFog where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

deepOneHatchling :: CardDef
deepOneHatchling =
  (enemy "07255" "Deep One Hatchling" ALightInTheFog 4)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Surge]
    }

deepOneNursemaid :: CardDef
deepOneNursemaid =
  (enemy "07254" "Deep One Nursemaid" ALightInTheFog 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
    }

oceirosMarsh :: CardDef
oceirosMarsh =
  (enemy "07253" ("Oceiros Marsh" <:> "Keeper of the Lighthouse") ALightInTheFog 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, DeepOne, Hybrid, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }
