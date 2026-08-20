module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.InTooDeep where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

emergingDeepOne :: CardDef
emergingDeepOne =
  (enemy "07146" "Emerging Deep One" InTooDeep 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdRevelation = IsRevelation
    }

innsmouthShoggoth :: CardDef
innsmouthShoggoth =
  (enemy "07144" "Innsmouth Shoggoth" InTooDeep 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Shoggoth, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 1
    }

ravagerFromTheDeep :: CardDef
ravagerFromTheDeep =
  (enemy "07145" "Ravager from the Deep" InTooDeep 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }
