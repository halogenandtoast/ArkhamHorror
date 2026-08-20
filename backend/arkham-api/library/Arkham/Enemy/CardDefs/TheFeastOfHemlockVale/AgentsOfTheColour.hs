module Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.AgentsOfTheColour where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

miasmaticShadow :: CardDef
miasmaticShadow =
  (enemy "10724" "Miasmatic Shadow" AgentsOfTheColour 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Colour]
    , cdKeywords =
        setFromList
          [ Keyword.Aloof
          , Keyword.Hunter
          , Keyword.ScenarioModifierKeyword "time" (String "Night") Keyword.Elusive
          ]
    , cdVictoryPoints = Just 0
    }
