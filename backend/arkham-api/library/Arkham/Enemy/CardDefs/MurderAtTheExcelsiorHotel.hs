module Arkham.Enemy.CardDefs.MurderAtTheExcelsiorHotel where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

arkhamOfficer :: CardDef
arkhamOfficer =
  (enemy "84009" "Arkham Officer" MurderAtTheExcelsiorHotel 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Police, Innocent]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol LocationWithAnyClues]
    , cdVictoryPoints = Just 0
    }

conspicuousStaff :: CardDef
conspicuousStaff =
  (enemy "84021" "Conspicuous Staff" MurderAtTheExcelsiorHotel 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

hotelGuest :: CardDef
hotelGuest =
  (enemy "84022" "Hotel Guest" MurderAtTheExcelsiorHotel 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Guest, Innocent]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithTrait CrimeScene)]
    , cdVictoryPoints = Just 0
    }

mrTrombly :: CardDef
mrTrombly =
  (enemy "84020" ("Mr. Trombly" <:> "Maddened Concierge") MurderAtTheExcelsiorHotel 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 5
    , cdCardTraits = setFromList [Humanoid, Staff]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }
