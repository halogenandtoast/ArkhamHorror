module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.Myconids where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

blackAmanita :: CardDef
blackAmanita =
  (enemy "10738" "Black Amanita" Myconids 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Flora, Mutated]
    , cdKeywords =
        setFromList
          [ Keyword.ScenarioModifierKeyword "time" (String "Day") Keyword.Aloof
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Massive
          ]
    }

corpseLichen :: CardDef
corpseLichen =
  (enemy "10739" "Corpse Lichen" Myconids 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Flora, Mutated]
    , cdKeywords =
        setFromList
          [ Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Hunter
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Alert
          ]
    }
