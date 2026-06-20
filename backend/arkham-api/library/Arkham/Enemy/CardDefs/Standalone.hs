{- HLINT ignore "Use camelCase" -}
module Arkham.Enemy.CardDefs.Standalone where

import Arkham.Enemy.CardDefs.Import
import Arkham.Keyword qualified as Keyword

tommyMalloy :: CardDef
tommyMalloy =
  unique
    $ (weakness "60103" "Tommy Malloy")
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
      , cdKeywords = setFromList [Keyword.Hunter]
      }

valeriyaAntonovaDontMessWithHer :: CardDef
valeriyaAntonovaDontMessWithHer =
  unique
    $ doubleSided "71016"
    $ (enemy "71016b" ("Valeriya Antonova" <:> "Don't Mess With Her") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

rookieCop :: CardDef
rookieCop =
  (enemy "71020" "Rookie Cop" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Police, Rival]
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Aloof, Keyword.Hunter]
    }

caldwellPhilipsCompelledByDreams :: CardDef
caldwellPhilipsCompelledByDreams =
  unique
    $ doubleSided "71022"
    $ (enemy "71022b" ("Caldwell Philips" <:> "Compelled by Dreams") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

johnnyValoneHereToCollect :: CardDef
johnnyValoneHereToCollect =
  unique
    $ doubleSided "71028"
    $ (enemy "71028b" ("Johnny Valone" <:> "Here to Collect") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

carlSanfordIntimidatingPresence :: CardDef
carlSanfordIntimidatingPresence =
  unique
    $ doubleSided "71034"
    $ (enemy "71034b" ("Carl Sanford" <:> "Intimidating Presence") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

williamBainDefiantToTheLast :: CardDef
williamBainDefiantToTheLast =
  unique
    $ doubleSided "71040"
    $ (enemy "71040b" ("William Bain" <:> "Defiant to the Last") TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [Humanoid, Leader, Rival, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

theBloodlessMan :: CardDef
theBloodlessMan =
  unique
    $ doubleSided "71045b"
    $ (enemy "71045" "The Bloodless Man" TheMidwinterGala 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Humanoid, LanternClub, Elite]
      , cdKeywords =
          setFromList
            [ Keyword.Aloof
            , Keyword.Patrol (LocationWithAsset $ AssetWithTrait Guest)
            ]
      }

theBloodlessManUnleashed :: CardDef
theBloodlessManUnleashed =
  unique
    $ doubleSided "71045"
    $ (enemy "71045b" "The Bloodless Man" TheMidwinterGala 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Monster, Abomination, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
      , cdVictoryPoints = Just 1
      }

enragedGug :: CardDef
enragedGug =
  (enemy "71047" "Enraged Gug" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dreamlands, Gug]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter]
    }

abhorrentMoonBeast :: CardDef
abhorrentMoonBeast =
  (enemy "71048" "Abhorrent Moon-Beast" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dreamlands, Servitor]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

savageShantak :: CardDef
savageShantak =
  (enemy "71049" "Savage Shantak" TheMidwinterGala 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 1
    , cdEvade = evade 1
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Dreamlands, Shantak]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

declanPearce :: CardDef
declanPearce =
  unique
    $ (enemy "71051" "Declan Pearce" TheMidwinterGala 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, LanternClub, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 0
      }

lanternClubMember :: CardDef
lanternClubMember =
  (enemy "71053" "Lantern Club Member" TheMidwinterGala 4)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, LanternClub]
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
      , cdCardTraits = setFromList [Creature, Dinosaur, Elite]
      , cdKeywords = setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Massive, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
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

bogGator :: CardDef
bogGator =
  (enemy "81022" "Bog Gator" TheBayou 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Creature]
    }

swampLeech :: CardDef
swampLeech =
  (enemy "81023" "Swamp Leech" TheBayou 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdHealth = health 1
    , cdCardTraits = setFromList [Creature]
    }

theRougarou :: CardDef
theRougarou =
  unique
    $ (enemy "81028" ("The Rougarou" <:> "Cursed Soul") CurseOfTheRougarou 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, Creature, Elite]
      , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
      }

slimeCoveredDhole :: CardDef
slimeCoveredDhole =
  (enemy "81031" "Slime-Covered Dhole" CurseOfTheRougarou 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dhole]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

marshGug :: CardDef
marshGug =
  (enemy "81032" "Marsh Gug" CurseOfTheRougarou 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Gug]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

darkYoungHost :: CardDef
darkYoungHost =
  (enemy "81033" "Dark Young Host" CurseOfTheRougarou 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, DarkYoung]
    , cdVictoryPoints = Just 1
    }

balefulReveler :: CardDef
balefulReveler =
  unique
    $ doubleSided "82002"
    $ (enemy "82002b" ("Baleful Reveler" <:> "Spreading Chaos") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

donLagorio :: CardDef
donLagorio =
  unique
    $ doubleSided "82017b"
    $ (enemy "82017" ("Don Lagorio" <:> "Secret Servant") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

elisabettaMagro :: CardDef
elisabettaMagro =
  unique
    $ doubleSided "82018b"
    $ ( enemy
          "82018"
          ("Elisabetta Magro" <:> "High Servant of the Order")
          CarnevaleOfHorrors
          1
      )
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Lodge, Elite]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 1
      }

salvatoreNeri :: CardDef
salvatoreNeri =
  unique
    $ doubleSided "82019b"
    $ ( enemy
          "82019"
          ("Salvatore Neri" <:> "Master of Illusions")
          CarnevaleOfHorrors
          1
      )
      { cdSanityDamage = sanityDamage 2
      , cdFight = fightUnknown
      , cdEvade = evadeUnknown
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Sorcerer, Elite]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

savioCorvi :: CardDef
savioCorvi =
  unique
    $ doubleSided "82020b"
    $ (enemy "82020" ("Savio Corvi" <:> "Dark Lurker") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 5
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

cnidathqua :: CardDef
cnidathqua =
  unique
    $ (enemy "82027" ("Cnidathqua" <:> "The Many-armed Beast") CarnevaleOfHorrors 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdHealth = healthPerInvestigator 8
      , cdCardTraits = setFromList [Monster, AncientOne, Elite]
      }

poleman :: CardDef
poleman =
  (enemy "82028" "Poleman" CarnevaleOfHorrors 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, DeepOne]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

carnevaleSentinel :: CardDef
carnevaleSentinel =
  (enemy "82029" "Carnevale Sentinel" CarnevaleOfHorrors 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

writhingAppendage :: CardDef
writhingAppendage =
  (enemy "82030" "Writhing Appendage" CarnevaleOfHorrors 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Tentacle]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

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

otherworldlyMeddler :: CardDef
otherworldlyMeddler =
  (enemy "84029" ("Otherworldly Meddler" <:> "Presence from Beyond the Stars") AlienInterference 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 5
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdKeywords = setFromList [Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

hotelManager :: CardDef
hotelManager =
  (enemy "84032" ("Hotel Manager" <:> "Let the Feast Begin") ExcelsiorManagement 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 6
    , cdCardTraits = setFromList [Monster, Staff, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

hotelSecurity :: CardDef
hotelSecurity =
  (enemy "84033" "Hotel Security" ExcelsiorManagement 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster, Staff]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

dimensionalShambler :: CardDef
dimensionalShambler =
  (enemy "84035" "Dimensional Shambler" DarkRituals 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Extradimensional, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 2
    }

cultistOfTheEnclave :: CardDef
cultistOfTheEnclave =
  (enemy "84036" "Cultist of the Enclave" DarkRituals 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

vengefulSpecter :: CardDef
vengefulSpecter =
  (enemy "84041" ("Vengeful Specter" <:> "The First Victim") SinsOfThePast 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 5
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Geist, Elite]
    , cdKeywords = setFromList [Keyword.Patrol "Room 245", Keyword.Retaliate]
    , cdVictoryPoints = Just 2
    , cdUnique = True
    }

abarranArrigorriagakoaTheManWithTheRubyRing :: CardDef
abarranArrigorriagakoaTheManWithTheRubyRing =
  doubleSided "88034b"
    $ (enemy "88034a" ("Abarran Arrigorriagakoa" <:> "The Man with the RUby Ring") FortuneAndFolly 1)
      { cdCardTraits = setFromList [Humanoid, Casino, Coterie, Elite]
      , cdKeywords =
          setFromList [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "abarranNext"))]
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "ace")]
      , cdUnique = True
      }

abarranArrigorriagakoaAbarranUnleashed :: CardDef
abarranArrigorriagakoaAbarranUnleashed =
  doubleSided "88034a"
    $ (enemy "88034b" ("Abarran Arrigorriagakoa" <:> "Abarran Unleashe") FortuneAndFolly 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Casino, Coterie, Elite]
      , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "ace")]
      , cdUnique = True
      , cdVictoryPoints = Just 1
      }

casinoGuardA :: CardDef
casinoGuardA =
  (enemy "88035a" "Casino Guard" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "casinoGuardANext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "queen")]
    }

casinoGuardB :: CardDef
casinoGuardB =
  (enemy "88035b" "Casino Guard" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "casinoGuardBNext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "jack")]
    }

casinoGuardC :: CardDef
casinoGuardC =
  (enemy "88035c" "Casino Guard" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "casinoGuardCNext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "ten")]
    }

houseDealerA :: CardDef
houseDealerA =
  (enemy "88036a" "House Dealer" FortuneAndFolly 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "houseDealerANext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "nine")]
    }

houseDealerB :: CardDef
houseDealerB =
  (enemy "88036b" "House Dealer" FortuneAndFolly 1)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "houseDealerBNext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "nine")]
    }

securityPatrolA :: CardDef
securityPatrolA =
  (enemy "88037a" "Security Patrol" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "securityPatrolANext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "queen")]
    }

securityPatrolB :: CardDef
securityPatrolB =
  (enemy "88037b" "Security Patrol" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "securityPatrolBNext"))]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "jack")]
    }

securityPatrolC :: CardDef
securityPatrolC =
  (enemy "88037c" "Security Patrol" FortuneAndFolly 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Casino]
    , cdKeywords =
        setFromList
          [Keyword.Aloof, Keyword.Patrol (LocationWithModifier (ScenarioModifier "securityPatrolCNext"))]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "ten")]
    }

fortunesShieldA :: CardDef
fortunesShieldA =
  (enemy "88046a" "Fortune's Shield" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesShieldANext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "king")]
    }

fortunesShieldB :: CardDef
fortunesShieldB =
  (enemy "88046b" "Fortune's Shield" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesShieldBNext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "king")]
    }

fortunesDaggerA :: CardDef
fortunesDaggerA =
  (enemy "88047a" "Fortune's Dagger" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesDaggerANext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "king")]
    }

fortunesDaggerB :: CardDef
fortunesDaggerB =
  (enemy "88047b" "Fortune's Dagger" FortunesChosen 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Casino, Cultist]
    , cdKeywords =
        setFromList
          [Keyword.Patrol (LocationWithModifier (ScenarioModifier "fortunesDaggerBNext")), Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "king")]
    }

dimensionalShamblerHunterFromBeyond :: CardDef
dimensionalShamblerHunterFromBeyond =
  (enemy "88048" ("Dimensional Shambler" <:> "Hunter from Beyond") PlanInShambles 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 6
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Extradimensional, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "ace")]
    , cdVictoryPoints = Just 1
    }

dimensionalDuplicatorA :: CardDef
dimensionalDuplicatorA =
  (enemy "88049a" "Dimensional Duplicator" PlanInShambles 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdMeta = mapFromList [("suit", String "spades"), ("value", String "six")]
    }

dimensionalDuplicatorB :: CardDef
dimensionalDuplicatorB =
  (enemy "88049b" "Dimensional Duplicator" PlanInShambles 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdMeta = mapFromList [("suit", String "clubs"), ("value", String "six")]
    }

dimensionalDisplacerA :: CardDef
dimensionalDisplacerA =
  (enemy "88050a" "Dimensional Displacer" PlanInShambles 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "diamonds"), ("value", String "six")]
    }

dimensionalDisplacerB :: CardDef
dimensionalDisplacerB =
  (enemy "88050b" "Dimensional Displacer" PlanInShambles 1)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    , cdMeta = mapFromList [("suit", String "hearts"), ("value", String "six")]
    }

vengefulShade :: CardDef
vengefulShade =
  (weakness "90053" "Vengeful Shade")
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 5
    , cdEvade = evade 5
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Geist]
    , cdKeywords = singleton Keyword.Hunter
    }

serpentsOfYigAdvanced :: CardDef
serpentsOfYigAdvanced =
  (weakness "90083" "Serpents of Yig")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Humanoid, Monster, Serpent]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Advanced]
    , cdRevelation = IsRevelation
    }

felineHybrid :: CardDef
felineHybrid =
  unique
    $ (weakness "60553" "Feline Hybrid")
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Creature, Mutated]
      , cdKeywords = setFromList [Keyword.Elusive, Keyword.Hunter]
      }

bloodDrinker :: CardDef
bloodDrinker =
  (basicWeakness "60554" "Blood Drinker")
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Humanoid, Monster]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
    }

-- The Blob That Ate Everything

oozeling :: CardDef
oozeling =
  (enemy "85039" "Oozeling" TheBlobThatAteEverything 4)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.ScenarioKeywordX "Blob" 1]
    }

graspingOoze :: CardDef
graspingOoze =
  (enemy "85040" "Grasping Ooze" TheBlobThatAteEverything 2)
    { cdSanityDamage = sanityDamage 2
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.ScenarioKeywordX "Blob" 3]
    }

cubicOoze :: CardDef
cubicOoze =
  (enemy "85041" "Cubic Ooze" TheBlobThatAteEverything 2)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 1
    , cdEvade = evade 4
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords = setFromList [Keyword.ScenarioKeywordX "Blob" 2]
    , cdRevelation = IsRevelation
    }

oozewraith :: CardDef
oozewraith =
  (enemy "85042" "Oozewraith" TheBlobThatAteEverything 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Ooze, Manifold]
    , cdKeywords =
        setFromList [Keyword.Alert, Keyword.Hunter, Keyword.Retaliate, Keyword.ScenarioKeywordX "Blob" 5]
    , cdVictoryPoints = Just 1
    }

vulnerableHeart :: CardDef
vulnerableHeart =
  (enemy "85043" "Vulnerable Heart" TheBlobThatAteEverything 1)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 2
    , cdFight = fightX
    , cdCardTraits = setFromList [Monster, Ooze, Elite]
    , cdKeywords = setFromList [Keyword.Massive, Keyword.Retaliate]
    }

subject8L08 :: CardDef
subject8L08 =
  (enemy "85038" "Subject 8L-08" BlobSingleGroup 1)
    { cdHealth = healthPerInvestigator 15
    , cdCardTraits = setFromList [Monster, Ooze, Elite]
    }

subject8L08EpicMultiplayer :: CardDef
subject8L08EpicMultiplayer =
  (enemy "85037" "Subject 8L-08" BlobEpicMultiplayer 1)
    { cdHealth = healthStar
    , cdCardTraits = setFromList [Monster, Ooze, Elite]
    }

-- Mi-Go Incursion

miGoGeneral :: CardDef
miGoGeneral =
  (enemy "85027" "Mi-Go General" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 4
    , cdCardTraits = setFromList [Monster, Servitor, MiGo, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

miGoDrone :: CardDef
miGoDrone =
  (enemy "85033" "Mi-Go Drone" MiGoIncursion 3)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 1
    , cdCardTraits = setFromList [Monster, MiGo]
    , cdKeywords = setFromList [Keyword.Surge]
    }

miGoHarvester :: CardDef
miGoHarvester =
  (enemy "85034" "Mi-Go Harvester" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdVictoryPoints = Just 1
    }

miGoMeddler :: CardDef
miGoMeddler =
  (enemy "85035" "Mi-Go Meddler" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 3
    , cdEvade = evade 4
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdVictoryPoints = Just 1
    }

miGoAbductor :: CardDef
miGoAbductor =
  (enemy "85036" "Mi-Go Abductor" MiGoIncursion 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = healthPerInvestigator 2
    , cdCardTraits = setFromList [Monster, MiGo, Elite]
    , cdVictoryPoints = Just 1
    }
neith :: CardDef
neith =
  unique
    $ (enemy "83013" ("Neith" <:> "Harbinger of the Abyss") TheEternalSlumber 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Humanoid, Brotherhood, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

humbleSupplicant :: CardDef
humbleSupplicant =
  (enemy "83014" "Humble Supplicant" TheEternalSlumber 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    , cdKeywords = setFromList [Keyword.Aloof]
    }

creatureFromTheAbyss :: CardDef
creatureFromTheAbyss =
  (enemy "83015" "Creature from the Abyss" TheEternalSlumber 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fightX
    , cdEvade = evade 3
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Dreamlands]
    , cdKeywords = setFromList [Keyword.Retaliate]
    }

xzharah :: CardDef
xzharah =
  unique
    $ doubleSided "83027b"
    $ (enemy "83027a" ("Xzharah" <:> "Chosen of the Beast") TheNightsUsurper 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 5
      , cdCardTraits = setFromList [Servitor, Dreamlands, Conspirator, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      }

dreadedShantak :: CardDef
dreadedShantak =
  (enemy "83029" "Dreaded Shantak" TheNightsUsurper 2)
    { cdHealthDamage = healthDamage 2
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 5
    , cdEvade = evade 3
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster, Dreamlands, Shantak]
    , cdKeywords = setFromList [Keyword.Hunter, Keyword.Massive]
    }

speakerForTheDarkPharaoh :: CardDef
speakerForTheDarkPharaoh =
  (enemy "83030" "Speaker for the Dark Pharaoh" TheNightsUsurper 2)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Cultist]
    }

drLaylaElMasri :: CardDef
drLaylaElMasri =
  unique
    $ doubleSided "83031b"
    $ (enemy "83031a" ("Dr. Layla El Masri" <:> "Hieratic Translator") BrotherhoodOfTheBeast 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

drWentworthMoore :: CardDef
drWentworthMoore =
  unique
    $ doubleSided "83032b"
    $ (enemy "83032a" ("Dr. Wentworth Moore" <:> "Dark Supplicant") BrotherhoodOfTheBeast 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

nadiaNimr :: CardDef
nadiaNimr =
  unique
    $ doubleSided "83033b"
    $ (enemy "83033a" ("Nadia Nimr" <:> "Priestess of the Beast") BrotherhoodOfTheBeast 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

farid :: CardDef
farid =
  unique
    $ doubleSided "83034b"
    $ (enemy "83034a" ("Farid" <:> "Seedy Salesman") BrotherhoodOfTheBeast 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

nassor :: CardDef
nassor =
  unique
    $ doubleSided "83035b"
    $ (enemy "83035a" ("Nassor" <:> "Brotherhood Operative") BrotherhoodOfTheBeast 1)
      { cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdKeywords = setFromList [Keyword.Retaliate]
      , cdVictoryPoints = Just 1
      }

professorNathanielTaylor :: CardDef
professorNathanielTaylor =
  unique
    $ doubleSided "83036b"
    $ (enemy "83036a" ("Professor Nathaniel Taylor" <:> "Keeper of Antiquities") BrotherhoodOfTheBeast 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist, Brotherhood]
      , cdVictoryPoints = Just 1
      }

abyssalRevenant :: CardDef
abyssalRevenant =
  (enemy "83045" "Abyssal Revenant" SandsOfEgypt 2)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 4
    , cdEvade = evade 2
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Dreamlands]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

thingInTheSarcophagus :: CardDef
thingInTheSarcophagus =
  (enemy "83046" "Thing in the Sarcophagus" SandsOfEgypt 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 5
    , cdCardTraits = setFromList [Monster]
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

edwinBennetBitterAdversary :: CardDef
edwinBennetBitterAdversary =
  unique
    $ (enemy "87036a" ("Edwin Bennet" <:> "Bitter Adversary") MachinationsThroughTimeSingleGroup 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords =
          setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
      , cdDoubleSided = True
      , cdOtherSide = Just "87036b"
      , cdArt = "87036"
      }

edwinBennetEnviousRival :: CardDef
edwinBennetEnviousRival =
  unique
    $ (enemy "87037a" ("Edwin Bennet" <:> "Envious Rival") MachinationsThroughTimeEpicMultiplayer 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Humanoid, Elite]
      , cdKeywords =
          setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
      , cdDoubleSided = True
      , cdOtherSide = Just "87037b"
      , cdArt = "87037"
      }

oldSadieSheldon :: CardDef
oldSadieSheldon =
  unique
    $ (enemy "87040" ("Old Sadie Sheldon" <:> "Bootlegging Kingpin") MachinationsThroughTime 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 1
      , cdEvade = evade 1
      , cdHealth = health 1
      , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate]
      , cdKeywords = setFromList [Keyword.Aloof]
      , cdVictoryPoints = Just 0
      }

sheldonGang :: CardDef
sheldonGang =
  (enemy "87041" "Sheldon Gang" MachinationsThroughTime 3)
    { cdHealthDamage = healthDamage 2
    , cdFight = fight 3
    , cdEvade = evade 2
    , cdHealth = health 1
    , cdCardTraits = setFromList [Humanoid, Criminal, Syndicate, Elite]
    , cdKeywords = setFromList [Keyword.Surge, Keyword.Hunter, Keyword.Massive]
    , cdVictoryPoints = Just 0
    }

tyrthrha :: CardDef
tyrthrha =
  unique
    $ (enemy "87043" ("Tyr'thrha" <:> "Arch-Lord of Tindalos") MachinationsThroughTime 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthStar
      , cdCardTraits = setFromList [AncientOne, Tindalos, Elite]
      , cdKeywords =
          setFromList
            [ Keyword.Massive
            , Keyword.Retaliate
            , Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)
            ]
      , cdVictoryPoints = Just 0
      }

ghastlySatyr :: CardDef
ghastlySatyr =
  (enemy "87044" "Ghastly Satyr" MachinationsThroughTime 3)
    { cdHealthDamage = healthDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
    }

houndOfTindalos :: CardDef
houndOfTindalos =
  (enemy "87045" "Hound of Tindalos" MachinationsThroughTime 4)
    { cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 3
    , cdHealth = health 2
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords =
        setFromList [Keyword.Patrol (LocationWithAsset $ AssetWithTrait Scientist)]
    }

manyAngledThing :: CardDef
manyAngledThing =
  (enemy "87046" "Many-Angled Thing" MachinationsThroughTime 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 7
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos, Elite]
    , cdKeywords = setFromList [Keyword.Hunter]
    , cdVictoryPoints = Just 1
    }

tindalosAlphaMachinationsThroughTime :: CardDef
tindalosAlphaMachinationsThroughTime =
  (enemy "87047" "Tindalos Alpha" MachinationsThroughTime 1)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 4
    , cdEvade = evade 4
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, Extradimensional, Tindalos]
    , cdKeywords = setFromList [Keyword.Alert, Keyword.Retaliate]
    }

eixodolon :: CardDef
eixodolon =
  unique
    $ (enemy "70048" ("Eixodolon" <:> "Your Anguish Is My Power") TheLabyrinthsOfLunacy 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 6
      , cdCardTraits = setFromList [Humanoid, Servitor, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 3
      }

eixodolonsPetEpicMultiplayer :: CardDef
eixodolonsPetEpicMultiplayer =
  unique
    $ ( enemy
          "70049"
          ("Eixodolon's Pet" <:> "You Are Not Its First Victim, Or Its Last")
          LabyrinthsOfLunacyEpicMultiplayer
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

eixodolonsPet :: CardDef
eixodolonsPet =
  unique
    $ ( enemy
          "70050"
          ("Eixodolon's Pet" <:> "You Are Not Its First Victim, Or Its Last")
          LabyrinthsOfLunacySingleGroup
          1
      )
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter, Keyword.Retaliate]
      , cdVictoryPoints = Just 2
      }

theJailor :: CardDef
theJailor =
  unique
    $ (enemy "70051" "The Jailor" LabyrinthsOfLunacyEpicMultiplayer 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 12
      , cdCardTraits = setFromList [Monster, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 5
      }

facelessAbductor :: CardDef
facelessAbductor =
  (enemy "70052" "Faceless Abductor" TheLabyrinthsOfLunacy 2)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 2
    , cdFight = fight 4
    , cdEvade = evade 3
    , cdHealth = health 4
    , cdCardTraits = setFromList [Monster, Nightgaunt]
    , cdKeywords = setFromList [Keyword.Hunter]
    }

torturedVictim :: CardDef
torturedVictim =
  (enemy "70053" "Tortured Victim" TheLabyrinthsOfLunacy 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 2
    , cdEvade = evade 2
    , cdHealth = health 2
    , cdCardTraits = setFromList [Humanoid, Extradimensional]
    , cdRevelation = IsRevelation
    }

miGoGuard :: CardDef
miGoGuard =
  (enemy "70054" "Mi-Go Guard" TheLabyrinthsOfLunacy 3)
    { cdHealthDamage = healthDamage 1
    , cdSanityDamage = sanityDamage 1
    , cdFight = fight 3
    , cdEvade = evade 1
    , cdHealth = health 3
    , cdCardTraits = setFromList [Monster, MiGo]
    }
