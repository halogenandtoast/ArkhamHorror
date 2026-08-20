module Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.FogOverInnsmouth where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

wingedOne :: CardDef
wingedOne =
  (enemy "07094" "Winged One" FogOverInnsmouth 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evadeX
    , cdHealth = health 5
    , cdCardTraits = setFromList [Creature, Monster]
    , cdKeywords = singleton Keyword.Hunter
    , cdVictoryPoints = Just 1
    }
