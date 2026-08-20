{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.TheDreamEaters.WeaverOfTheCosmos where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

atlachNacha :: CardDef
atlachNacha =
  unique
    $ doubleSided "06346b"
    $ (enemy "06346" ("Atlach-Nacha" <:> "The Spider God") WeaverOfTheCosmos 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [AncientOne, Spider, Elite]
      , cdKeywords = singleton Keyword.Massive
      , cdVictoryPoints = Just 1
      }

legsOfAtlachNacha_347 :: CardDef
legsOfAtlachNacha_347 =
  (enemy "06347" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_348 :: CardDef
legsOfAtlachNacha_348 =
  (enemy "06348" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_349 :: CardDef
legsOfAtlachNacha_349 =
  (enemy "06349" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

legsOfAtlachNacha_350 :: CardDef
legsOfAtlachNacha_350 =
  (enemy "06350" "Legs of Atlach-Nacha" WeaverOfTheCosmos 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 3
    , cdCardTraits = setFromList [AncientOne, Spider, Elite]
    , cdKeywords = singleton Keyword.Massive
    , cdVictoryPoints = Just 1
    }

spiderOfLeng :: CardDef
spiderOfLeng =
  (enemy "06101" "Spider of Leng" Spiders 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Spider]
    }

swarmOfSpiders :: CardDef
swarmOfSpiders =
  (enemy "06102" "Swarm of Spiders" Spiders 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evadeX
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Spider]
    , cdKeywords = setFromList [Keyword.Swarming (Static 2)]
    }

webSpinner :: CardDef
webSpinner =
  (enemy "06351" "Web-Spinner" WeaverOfTheCosmos 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Spider]
    , cdKeywords = singleton Keyword.Aloof
    }
