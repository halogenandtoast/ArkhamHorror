module Arkham.Enemy.CardDefs.TheScarletKeys.DealingsInTheDark where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

sinisterAspirantA :: CardDef
sinisterAspirantA =
  (enemy "09586a" "Sinister Aspirant (A)" DealingsInTheDark 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert]
    }

sinisterAspirantB :: CardDef
sinisterAspirantB =
  (enemy "09586b" "Sinister Aspirant (B)" DealingsInTheDark 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert]
    }

sinisterAspirantC :: CardDef
sinisterAspirantC =
  (enemy "09586c" "Sinister Aspirant (C)" DealingsInTheDark 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Alert]
    }

umbralHarbinger :: CardDef
umbralHarbinger =
  (enemy "09585" "Umbral Harbinger" DealingsInTheDark 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Yuggoth]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
