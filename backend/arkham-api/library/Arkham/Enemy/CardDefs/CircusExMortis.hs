module Arkham.Enemy.CardDefs.CircusExMortis where

import Arkham.Enemy.CardDefs.Import

-- one_night_only
disguisedMonstrosityCircusExMortis :: CardDef
disguisedMonstrosityCircusExMortis =
  (enemy "z-circus-ex-mortis-013" "Disguised Monstrosity" CircusExMortisOneNightOnly 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Creature, Monster, Abomination, Elite]
      , cdVictoryPoints = Just 1
      }

grotesqueLionCircusExMortis :: CardDef
grotesqueLionCircusExMortis =
  (enemy "z-circus-ex-mortis-014" "Grotesque Lion" CircusExMortisOneNightOnly 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      }

newMoonDrudgeCircusExMortis :: CardDef
newMoonDrudgeCircusExMortis =
  (enemy "z-circus-ex-mortis-016" "New Moon Drudge" CircusExMortisOneNightOnly 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

-- the_primrose_path
newMoonIllusionistCircusExMortis :: CardDef
newMoonIllusionistCircusExMortis =
  (enemy "z-circus-ex-mortis-037" "New Moon Illusionist" CircusExMortisThePrimrosePath 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Performer, Elite]
      , cdVictoryPoints = Just 1
      }

circusPredatorCircusExMortis :: CardDef
circusPredatorCircusExMortis =
  (enemy "z-circus-ex-mortis-038" "Circus Predator" CircusExMortisThePrimrosePath 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      }

ursineBruteCircusExMortis :: CardDef
ursineBruteCircusExMortis =
  (enemy "z-circus-ex-mortis-041" "Ursine Brute" CircusExMortisThePrimrosePath 2)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = health 4
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      , cdVictoryPoints = Just 1
      }

-- harm_s_way
toweringDarkYoungCircusExMortis_067 :: CardDef
toweringDarkYoungCircusExMortis_067 =
  (enemy "z-circus-ex-mortis-067" "Towering Dark Young" CircusExMortisHarmsWay 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoungCircusExMortis_068 :: CardDef
toweringDarkYoungCircusExMortis_068 =
  (enemy "z-circus-ex-mortis-068" "Towering Dark Young" CircusExMortisHarmsWay 1)
      { cdSanityDamage = sanityDamage 3
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoungCircusExMortis_069 :: CardDef
toweringDarkYoungCircusExMortis_069 =
  (enemy "z-circus-ex-mortis-069" "Towering Dark Young" CircusExMortisHarmsWay 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoungCircusExMortis_070 :: CardDef
toweringDarkYoungCircusExMortis_070 =
  (enemy "z-circus-ex-mortis-070" "Towering Dark Young" CircusExMortisHarmsWay 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoungCircusExMortis_071 :: CardDef
toweringDarkYoungCircusExMortis_071 =
  (enemy "z-circus-ex-mortis-071" "Towering Dark Young" CircusExMortisHarmsWay 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

sacrificialShepherdCircusExMortis :: CardDef
sacrificialShepherdCircusExMortis =
  (enemy "z-circus-ex-mortis-074" "Sacrificial Shepherd" CircusExMortisHarmsWay 2)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

-- all_points_west
loomingGoatspawnCircusExMortis :: CardDef
loomingGoatspawnCircusExMortis =
  (enemy "z-circus-ex-mortis-100" "Looming Goatspawn" CircusExMortisAllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

rampagingGoatspawnCircusExMortis :: CardDef
rampagingGoatspawnCircusExMortis =
  (enemy "z-circus-ex-mortis-101" "Rampaging Goatspawn" CircusExMortisAllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

ravenousGoatspawnCircusExMortis :: CardDef
ravenousGoatspawnCircusExMortis =
  (enemy "z-circus-ex-mortis-102" "Ravenous Goatspawn" CircusExMortisAllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

writhingGoatspawnCircusExMortis :: CardDef
writhingGoatspawnCircusExMortis =
  (enemy "z-circus-ex-mortis-103" "Writhing Goatspawn" CircusExMortisAllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

newMoonStiltwalkerCircusExMortis :: CardDef
newMoonStiltwalkerCircusExMortis =
  (enemy "z-circus-ex-mortis-106" "New Moon Stiltwalker" CircusExMortisAllPointsWest 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonTumblerCircusExMortis :: CardDef
newMoonTumblerCircusExMortis =
  (enemy "z-circus-ex-mortis-107" "New Moon Tumbler" CircusExMortisAllPointsWest 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

-- piper_at_the_gates_of_dawn
sylvesterBlakeCircusExMortis :: CardDef
sylvesterBlakeCircusExMortis =
  doubleSided "z-circus-ex-mortis-120b"
      $ (enemy "z-circus-ex-mortis-120" ("Sylvester Blake" <:> "Ringmaster of the New Moon Circus") CircusExMortisPiperAtTheGatesOfDawn 1)
        { cdHealthDamage = healthDamage 1
        , cdSanityDamage = sanityDamage 1
        , cdFight = fight 3
        , cdEvade = evade 3
        , cdHealth = healthPerInvestigator 4
        , cdCardTraits = setFromList [Humanoid, Performer, Elite]
        }

-- bacchanalia
goatspawnCorruptorCircusExMortis :: CardDef
goatspawnCorruptorCircusExMortis =
  (enemy "z-circus-ex-mortis-145" "Goatspawn Corruptor" CircusExMortisBacchanalia 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Monster, Elite]
      }

brashLotharioCircusExMortis :: CardDef
brashLotharioCircusExMortis =
  (enemy "z-circus-ex-mortis-148" "Brash Lothario" CircusExMortisBacchanalia 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

partyAnimalCircusExMortis :: CardDef
partyAnimalCircusExMortis =
  (enemy "z-circus-ex-mortis-151" "Party Animal" CircusExMortisBacchanalia 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

sadisticSocialiteCircusExMortis :: CardDef
sadisticSocialiteCircusExMortis =
  (enemy "z-circus-ex-mortis-153" "Sadistic Socialite" CircusExMortisBacchanalia 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

struttingPeacockCircusExMortis :: CardDef
struttingPeacockCircusExMortis =
  (enemy "z-circus-ex-mortis-154" "Strutting Peacock" CircusExMortisBacchanalia 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

-- red_sunrise
theCultEnMasseLeaderlessFanaticismCircusExMortis :: CardDef
theCultEnMasseLeaderlessFanaticismCircusExMortis =
  (enemy "z-circus-ex-mortis-184" ("The Cult En Masse" <:> "Leaderless Fanaticism") CircusExMortisRedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdVictoryPoints = Just 1
      }

theCultEnMasseBlackGoatsRaptureCircusExMortis :: CardDef
theCultEnMasseBlackGoatsRaptureCircusExMortis =
  (enemy "z-circus-ex-mortis-185" ("The Cult En Masse" <:> "Black Goat's Rapture") CircusExMortisRedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdVictoryPoints = Just 1
      }

theCultEnMasseRingmastersFervorCircusExMortis :: CardDef
theCultEnMasseRingmastersFervorCircusExMortis =
  (enemy "z-circus-ex-mortis-186" ("The Cult En Masse" <:> "Ringmaster's Fervor") CircusExMortisRedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdVictoryPoints = Just 1
      }

devoteeOfTheThousandCircusExMortis :: CardDef
devoteeOfTheThousandCircusExMortis =
  (enemy "z-circus-ex-mortis-187" "Devotee of the Thousand" CircusExMortisRedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

maliciousGoatspawnCircusExMortis :: CardDef
maliciousGoatspawnCircusExMortis =
  (enemy "z-circus-ex-mortis-189" "Malicious Goatspawn" CircusExMortisRedSunrise 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Monster]
      }

roamingDarkYoungCircusExMortis :: CardDef
roamingDarkYoungCircusExMortis =
  (enemy "z-circus-ex-mortis-190" "Roaming Dark Young" CircusExMortisRedSunrise 2)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, DarkYoung]
      }

-- thousand_to_one
ravenousBroodCircusExMortis :: CardDef
ravenousBroodCircusExMortis =
  doubleSided "z-circus-ex-mortis-209b"
      $ (enemy "z-circus-ex-mortis-209" "Ravenous Brood" CircusExMortisThousandToOne 8)
        { cdHealthDamage = healthDamage 1
        , cdSanityDamage = sanityDamage 1
        , cdFight = fight 3
        , cdEvade = evade 2
        , cdCardTraits = setFromList [Monster, DarkYoung]
        }

darkYoungJuggernautCircusExMortis :: CardDef
darkYoungJuggernautCircusExMortis =
  (enemy "z-circus-ex-mortis-211" "Dark Young Juggernaut" CircusExMortisThousandToOne 2)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, DarkYoung]
      }

shubNiggurathCircusExMortis :: CardDef
shubNiggurathCircusExMortis =
  (enemy "z-circus-ex-mortis-215" ("Shub-Niggurath" <:> "All-Mother of a Thousand Young") CircusExMortisThousandToOne 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      }

-- children_of_the_goat
nascentDarkYoungCircusExMortis :: CardDef
nascentDarkYoungCircusExMortis =
  (enemy "z-circus-ex-mortis-217" "Nascent Dark Young" CircusExMortisChildrenOfTheGoat 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Monster, DarkYoung]
      }

twistedSatyrCircusExMortis :: CardDef
twistedSatyrCircusExMortis =
  (enemy "z-circus-ex-mortis-218" "Twisted Satyr" CircusExMortisChildrenOfTheGoat 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Monster]
      }

-- cult_of_shub-niggurath
supplicantOfTheGoatCircusExMortis :: CardDef
supplicantOfTheGoatCircusExMortis =
  (enemy "z-circus-ex-mortis-227" "Supplicant of the Goat" CircusExMortisCultOfShubNiggurath 3)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

-- lunatic_night
mooncalfCircusExMortis :: CardDef
mooncalfCircusExMortis =
  (enemy "z-circus-ex-mortis-246" "Mooncalf" CircusExMortisLunaticNight 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      }

-- new_moon_daredevils
newMoonAcrobatCircusExMortis :: CardDef
newMoonAcrobatCircusExMortis =
  (enemy "z-circus-ex-mortis-248" "New Moon Acrobat" CircusExMortisNewMoonDaredevils 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonBeastTamerCircusExMortis :: CardDef
newMoonBeastTamerCircusExMortis =
  (enemy "z-circus-ex-mortis-249" "New Moon Beast Tamer" CircusExMortisNewMoonDaredevils 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonStrongmanCircusExMortis :: CardDef
newMoonStrongmanCircusExMortis =
  (enemy "z-circus-ex-mortis-250" "New Moon Strongman" CircusExMortisNewMoonDaredevils 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 1
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

-- new_moon_entertainers
newMoonCarnyCircusExMortis :: CardDef
newMoonCarnyCircusExMortis =
  (enemy "z-circus-ex-mortis-252" "New Moon Carny" CircusExMortisNewMoonEntertainers 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonClownCircusExMortis :: CardDef
newMoonClownCircusExMortis =
  (enemy "z-circus-ex-mortis-253" "New Moon Clown" CircusExMortisNewMoonEntertainers 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonMagicianCircusExMortis :: CardDef
newMoonMagicianCircusExMortis =
  (enemy "z-circus-ex-mortis-254" "New Moon Magician" CircusExMortisNewMoonEntertainers 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }
