module Arkham.Enemy.CardDefs.ThePathToCarcosa.EchoesOfThePast where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

possessedOathspeaker :: CardDef
possessedOathspeaker =
  ( enemy
      "03140"
      ("Possessed Oathspeaker" <:> "A Damnable Fate")
      EchoesOfThePast
      1
  )
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Monster, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    }

seekerOfCarcosa :: CardDef
seekerOfCarcosa =
  (enemy "03144" "Seeker of Carcosa" EchoesOfThePast 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = singleton Keyword.Aloof
    }
