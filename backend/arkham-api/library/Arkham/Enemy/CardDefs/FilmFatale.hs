{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.FilmFatale where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

allosaurusIndomitablePredator :: CardDef
allosaurusIndomitablePredator =
  doubleSided "72044b"
    $ (enemy "72044" ("Allosaurus" <:> "Indomitable Predator") ForgottenIsland 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdCardTraits = setFromList [Creature, Dinosaur, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

allosaurusRampagingPredator :: CardDef
allosaurusRampagingPredator =
  doubleSided "72044"
    $ (enemy "72044b" ("Allosaurus" <:> "Rampaging Predator") ForgottenIsland 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = health 6
      , cdCardTraits = setFromList [Creature, Dinosaur, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

curiousMoonNosyNuisance :: CardDef
curiousMoonNosyNuisance =
  unique
    $ doubleSided "72036"
    $ (enemy "72036b" ("Curious Moon" <:> "Nosy Nuisance") CosmicJourney 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Satellite, Cosmos, Elite]
      , cdKeywords = singleton Keyword.Massive
      }

dromaeosaurus :: CardDef
dromaeosaurus =
  (enemy "72045" "Dromaeosaurus" ForgottenIsland 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature, Dinosaur]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    }

erikaStrandPossessedProducer :: CardDef
erikaStrandPossessedProducer =
  unique
    $ (enemy "72022" ("Erika Strand" <:> "Possessed Producer") FilmFatale 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Possessed, Elite]
      , cdVictoryPoints = Just 1
      }

ghostLight :: CardDef
ghostLight =
  (enemy "72023" "Ghost Light" FilmFatale 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = singleton Keyword.Hunter
    }

possessedExtra_19 :: CardDef
possessedExtra_19 =
  (enemy "72019" "Possessed Extra" FilmFatale 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Possessed]
    }

possessedExtra_20 :: CardDef
possessedExtra_20 =
  (enemy "72020" "Possessed Extra" FilmFatale 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Possessed]
    }

possessedExtra_21 :: CardDef
possessedExtra_21 =
  (enemy "72021" "Possessed Extra" FilmFatale 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Possessed]
    }

saturniteDrudgeMilitia :: CardDef
saturniteDrudgeMilitia =
  (enemy "72033" "Saturnite Drudge Militia" CosmicJourney 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Saturnite]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Retaliate]
    }

saturniteMonarchGraciousHost :: CardDef
saturniteMonarchGraciousHost =
  unique
    $ doubleSided "72032b"
    $ (enemy "72032" ("Saturnite Monarch" <:> "Gracious Host") CosmicJourney 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Saturnite, Elite]
      , cdKeywords = singleton Keyword.Aloof
      , cdVictoryPoints = Just 1
      }

saturniteMonarchInAnAlienLand :: CardDef
saturniteMonarchInAnAlienLand =
  unique
    $ doubleSided "72032"
    $ (enemy "72032b" ("Saturnite Monarch" <:> "In an Alien Land") CosmicJourney 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Monster, Saturnite, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 1
      }

theContessaEnraged :: CardDef
theContessaEnraged =
  doubleSided "72055"
    $ (enemy "72055b" ("The Contessa" <:> "Enraged") AbominableContessa 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Monster, Elite]
      , cdKeywords = singleton Keyword.Elusive
      , cdVictoryPoints = Just 1
      }

theContessaNeedlesslySmug :: CardDef
theContessaNeedlesslySmug =
  doubleSided "72055b"
    $ (enemy "72055" ("The Contessa" <:> "Needlessly Smug") AbominableContessa 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Monster, Elite]
      , cdKeywords = singleton Keyword.Elusive
      , cdVictoryPoints = Just 1
      }

vampireThrall :: CardDef
vampireThrall =
  (enemy "72056" "Vampire Thrall" AbominableContessa 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
    }

werewolf :: CardDef
werewolf =
  (enemy "72057" "Werewolf" AbominableContessa 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 6
    , cdCardTraits = setFromList [Humanoid, Monster, Creature]
    , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter, Keyword.Retaliate]
    , cdVictoryPoints = Just 0
    }
