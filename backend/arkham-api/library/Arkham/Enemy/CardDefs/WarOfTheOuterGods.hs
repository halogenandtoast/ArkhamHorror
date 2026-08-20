module Arkham.Enemy.CardDefs.WarOfTheOuterGods where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

bringerOfParadise :: CardDef
bringerOfParadise =
  (enemy "86044" "Bringer of Paradise" ChildrenOfParadise 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate, #warring]
    }

bringerOfParadiseWarOfTheOuterGods :: CardDef
bringerOfParadiseWarOfTheOuterGods =
  (enemy "86044a" "Bringer of Paradise" WarOfTheOuterGods 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Mutated]
    , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate, #warring]
    , cdArt = "86044"
    }

discipleOfTheSwarm :: CardDef
discipleOfTheSwarm =
  (enemy "86029" "Disciple of the Swarm" WarOfTheOuterGods 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof, #warring]
    }

droningHorde :: CardDef
droningHorde =
  (enemy "86048" "Droning Horde" SwarmOfAssimilation 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, Insect, Servitor, Elite]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Swarming (Static 6)]
    , cdVictoryPoints = Just 1
    }

etherealEntity :: CardDef
etherealEntity =
  (enemy "86038" "Ethereal Entity" DeathOfStars 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [#warring]
    }

etherealEntityWarOfTheOuterGods :: CardDef
etherealEntityWarOfTheOuterGods =
  (enemy "86038a" "Ethereal Entity" WarOfTheOuterGods 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [#warring]
    , cdArt = "86038"
    }

ezelZenRezl :: CardDef
ezelZenRezl =
  unique
    $ (enemy "86047" ("Ezel-zen-rezl" <:> "The Lord of Swarms") SwarmOfAssimilation 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 2
      }

horrificShoggoth :: CardDef
horrificShoggoth =
  (enemy "86043" "Horrific Shoggoth" ChildrenOfParadise 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Shoggoth, Mutated]
    , cdKeywords = setFromList [Keyword.Retaliate, #warring]
    }

huneStitchedHerald :: CardDef
huneStitchedHerald =
  (enemy "86037" "Hune-Stitched Herald" DeathOfStars 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster]
    , cdKeywords = setFromList [#warring]
    }

maghanArkat :: CardDef
maghanArkat =
  unique
    $ (enemy "86041" ("Magh'an Ark'at" <:> "The Child of Paradise") ChildrenOfParadise 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Mutated, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

nihilisticStargazer :: CardDef
nihilisticStargazer =
  (enemy "86025" "Nihilistic Stargazer" WarOfTheOuterGods 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof, #warring]
    }

silenus :: CardDef
silenus =
  unique
    $ (enemy "86035" ("Silenus" <:> "The Empty Sky") DeathOfStars 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 2
      }

theInescapableMaw :: CardDef
theInescapableMaw =
  unique
    $ (enemy "86036" ("The Inescapable Maw" <:> "Servant of Silenus") DeathOfStars 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 5
      , cdHealth = health 8
      , cdCardTraits = setFromList [Humanoid, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

trylogog :: CardDef
trylogog =
  (enemy "86049" "Trylogog" SwarmOfAssimilation 4)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Insect]
    , cdKeywords = setFromList [Keyword.Swarming (Static 2), #warring]
    }

trylogogWarOfTheOuterGods :: CardDef
trylogogWarOfTheOuterGods =
  (enemy "86049a" "Trylogog" WarOfTheOuterGods 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature, Insect]
    , cdKeywords = setFromList [Keyword.Swarming (Static 2), #warring]
    , cdArt = "86049"
    }

vileBroodmaster :: CardDef
vileBroodmaster =
  unique
    $ (enemy "86042" ("Vile Broodmaster" <:> "Grotesque Abomination") ChildrenOfParadise 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 7
      , cdCardTraits = setFromList [Monster, Servitor, Mutated, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

zealotOfParadise :: CardDef
zealotOfParadise =
  (enemy "86027" "Zealot of Paradise" WarOfTheOuterGods 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof, #warring]
    }
