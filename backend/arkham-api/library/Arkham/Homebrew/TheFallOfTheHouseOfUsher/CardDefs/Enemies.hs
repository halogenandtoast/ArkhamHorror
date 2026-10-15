module Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.Sets qualified as Set
import Arkham.Keyword qualified as Keyword

luminousPhantasm :: CardDef
luminousPhantasm =
  (enemy ":the-fall-of-the-house-of-usher:030" "Luminous Phantasm" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight 3
    , cdHealth = health 2
    , cdEvade = evade 3
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

scionOfUsher :: CardDef
scionOfUsher =
  (enemy ":the-fall-of-the-house-of-usher:042" "Scion of Usher" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = setFromList [Monster, Geist]
    , cdFight = fight 3
    , cdHealth = health 3
    , cdEvade = evade 2
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Alert]
    , cdSanityDamage = sanityDamage 1
    }

secretiveSteward :: CardDef
secretiveSteward =
  (enemy ":the-fall-of-the-house-of-usher:045" "Secretive Steward" Set.TheFallOfTheHouseOfUsher 2)
    { cdCardTraits = setFromList [Humanoid, Servitor]
    , cdFight = fight 4
    , cdHealth = health 2
    , cdEvade = evade 2
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Aloof]
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    }

servantOfTwoMasters :: CardDef
servantOfTwoMasters =
  (enemy ":the-fall-of-the-house-of-usher:047" "Servant of Two Masters" Set.TheFallOfTheHouseOfUsher 3)
    { cdCardTraits = setFromList [Humanoid, Cultist]
    , cdFight = fight 3
    , cdHealth = health 1
    , cdEvade = evade 3
    }

theThingFromTheTarn :: CardDef
theThingFromTheTarn =
  ( enemy
      ":the-fall-of-the-house-of-usher:050"
      ("The Thing From the Tarn" <:> "Ancestral Evil")
      Set.TheFallOfTheHouseOfUsher
      1
  )
    { cdCardTraits = setFromList [Monster, AncientOne, Elite]
    , cdFight = fight 2
    , cdHealth = healthPerInvestigator 6
    , cdEvade = evade 2
    , cdKeywords = singleton Keyword.Massive
    , cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }
