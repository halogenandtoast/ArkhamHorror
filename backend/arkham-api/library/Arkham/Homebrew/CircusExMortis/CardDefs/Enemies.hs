module Arkham.Homebrew.CircusExMortis.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.CircusExMortis.Sets qualified as Set

-- one_night_only
disguisedMonstrosity :: CardDef
disguisedMonstrosity =
  (enemy "z-circus-ex-mortis-013" "Disguised Monstrosity" Set.OneNightOnly 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Creature, Monster, Abomination, Elite]
      , cdVictoryPoints = Just 1
      }

grotesqueLion :: CardDef
grotesqueLion =
  (enemy "z-circus-ex-mortis-014" "Grotesque Lion" Set.OneNightOnly 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      }

newMoonDrudge :: CardDef
newMoonDrudge =
  (enemy "z-circus-ex-mortis-016" "New Moon Drudge" Set.OneNightOnly 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

-- the_primrose_path
newMoonIllusionist :: CardDef
newMoonIllusionist =
  (enemy "z-circus-ex-mortis-037" "New Moon Illusionist" Set.ThePrimrosePath 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Humanoid, Performer, Elite]
      , cdVictoryPoints = Just 1
      }

circusPredator :: CardDef
circusPredator =
  (enemy "z-circus-ex-mortis-038" "Circus Predator" Set.ThePrimrosePath 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      }

ursineBrute :: CardDef
ursineBrute =
  (enemy "z-circus-ex-mortis-041" "Ursine Brute" Set.ThePrimrosePath 2)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = health 4
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      , cdVictoryPoints = Just 1
      }

-- harm_s_way
toweringDarkYoung_067 :: CardDef
toweringDarkYoung_067 =
  (enemy "z-circus-ex-mortis-067" "Towering Dark Young" Set.HarmsWay 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoung_068 :: CardDef
toweringDarkYoung_068 =
  (enemy "z-circus-ex-mortis-068" "Towering Dark Young" Set.HarmsWay 1)
      { cdSanityDamage = sanityDamage 3
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoung_069 :: CardDef
toweringDarkYoung_069 =
  (enemy "z-circus-ex-mortis-069" "Towering Dark Young" Set.HarmsWay 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoung_070 :: CardDef
toweringDarkYoung_070 =
  (enemy "z-circus-ex-mortis-070" "Towering Dark Young" Set.HarmsWay 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

toweringDarkYoung_071 :: CardDef
toweringDarkYoung_071 =
  (enemy "z-circus-ex-mortis-071" "Towering Dark Young" Set.HarmsWay 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      }

sacrificialShepherd :: CardDef
sacrificialShepherd =
  (enemy "z-circus-ex-mortis-074" "Sacrificial Shepherd" Set.HarmsWay 2)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

-- all_points_west
loomingGoatspawn :: CardDef
loomingGoatspawn =
  (enemy "z-circus-ex-mortis-100" "Looming Goatspawn" Set.AllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

rampagingGoatspawn :: CardDef
rampagingGoatspawn =
  (enemy "z-circus-ex-mortis-101" "Rampaging Goatspawn" Set.AllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

ravenousGoatspawn :: CardDef
ravenousGoatspawn =
  (enemy "z-circus-ex-mortis-102" "Ravenous Goatspawn" Set.AllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

writhingGoatspawn :: CardDef
writhingGoatspawn =
  (enemy "z-circus-ex-mortis-103" "Writhing Goatspawn" Set.AllPointsWest 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 5
      , cdCardTraits = setFromList [Monster, DarkYoung, Elite]
      , cdVictoryPoints = Just 1
      }

newMoonStiltwalker :: CardDef
newMoonStiltwalker =
  (enemy "z-circus-ex-mortis-106" "New Moon Stiltwalker" Set.AllPointsWest 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonTumbler :: CardDef
newMoonTumbler =
  (enemy "z-circus-ex-mortis-107" "New Moon Tumbler" Set.AllPointsWest 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

-- piper_at_the_gates_of_dawn
sylvesterBlake :: CardDef
sylvesterBlake =
  doubleSided "z-circus-ex-mortis-120b"
      $ (enemy "z-circus-ex-mortis-120" ("Sylvester Blake" <:> "Ringmaster of the New Moon Circus") Set.PiperAtTheGatesOfDawn 1)
        { cdHealthDamage = healthDamage 1
        , cdSanityDamage = sanityDamage 1
        , cdFight = fight 3
        , cdEvade = evade 3
        , cdHealth = healthPerInvestigator 4
        , cdCardTraits = setFromList [Humanoid, Performer, Elite]
        }

-- bacchanalia
goatspawnCorruptor :: CardDef
goatspawnCorruptor =
  (enemy "z-circus-ex-mortis-145" "Goatspawn Corruptor" Set.Bacchanalia 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Monster, Elite]
      }

brashLothario :: CardDef
brashLothario =
  (enemy "z-circus-ex-mortis-148" "Brash Lothario" Set.Bacchanalia 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

partyAnimal :: CardDef
partyAnimal =
  (enemy "z-circus-ex-mortis-151" "Party Animal" Set.Bacchanalia 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

sadisticSocialite :: CardDef
sadisticSocialite =
  (enemy "z-circus-ex-mortis-153" "Sadistic Socialite" Set.Bacchanalia 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

struttingPeacock :: CardDef
struttingPeacock =
  (enemy "z-circus-ex-mortis-154" "Strutting Peacock" Set.Bacchanalia 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

-- red_sunrise
theCultEnMasseLeaderlessFanaticism :: CardDef
theCultEnMasseLeaderlessFanaticism =
  (enemy "z-circus-ex-mortis-184" ("The Cult En Masse" <:> "Leaderless Fanaticism") Set.RedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdVictoryPoints = Just 1
      }

theCultEnMasseBlackGoatsRapture :: CardDef
theCultEnMasseBlackGoatsRapture =
  (enemy "z-circus-ex-mortis-185" ("The Cult En Masse" <:> "Black Goat's Rapture") Set.RedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdVictoryPoints = Just 1
      }

theCultEnMasseRingmastersFervor :: CardDef
theCultEnMasseRingmastersFervor =
  (enemy "z-circus-ex-mortis-186" ("The Cult En Masse" <:> "Ringmaster's Fervor") Set.RedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 6
      , cdCardTraits = setFromList [Humanoid, Cultist, Elite]
      , cdVictoryPoints = Just 1
      }

devoteeOfTheThousand :: CardDef
devoteeOfTheThousand =
  (enemy "z-circus-ex-mortis-187" "Devotee of the Thousand" Set.RedSunrise 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 1
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

maliciousGoatspawn :: CardDef
maliciousGoatspawn =
  (enemy "z-circus-ex-mortis-189" "Malicious Goatspawn" Set.RedSunrise 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Monster]
      }

roamingDarkYoung :: CardDef
roamingDarkYoung =
  (enemy "z-circus-ex-mortis-190" "Roaming Dark Young" Set.RedSunrise 2)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, DarkYoung]
      }

-- thousand_to_one
ravenousBrood :: CardDef
ravenousBrood =
  doubleSided "z-circus-ex-mortis-209b"
      $ (enemy "z-circus-ex-mortis-209" "Ravenous Brood" Set.ThousandToOne 8)
        { cdHealthDamage = healthDamage 1
        , cdSanityDamage = sanityDamage 1
        , cdFight = fight 3
        , cdEvade = evade 2
        , cdCardTraits = setFromList [Monster, DarkYoung]
        }

darkYoungJuggernaut :: CardDef
darkYoungJuggernaut =
  (enemy "z-circus-ex-mortis-211" "Dark Young Juggernaut" Set.ThousandToOne 2)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, DarkYoung]
      }

shubNiggurath :: CardDef
shubNiggurath =
  (enemy "z-circus-ex-mortis-215" ("Shub-Niggurath" <:> "All-Mother of a Thousand Young") Set.ThousandToOne 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      }

-- children_of_the_goat
nascentDarkYoung :: CardDef
nascentDarkYoung =
  (enemy "z-circus-ex-mortis-217" "Nascent Dark Young" Set.ChildrenOfTheGoat 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Monster, DarkYoung]
      }

twistedSatyr :: CardDef
twistedSatyr =
  (enemy "z-circus-ex-mortis-218" "Twisted Satyr" Set.ChildrenOfTheGoat 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Monster]
      }

-- cult_of_shub-niggurath
supplicantOfTheGoat :: CardDef
supplicantOfTheGoat =
  (enemy "z-circus-ex-mortis-227" "Supplicant of the Goat" Set.CultOfShubNiggurath 3)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Cultist]
      }

-- lunatic_night
mooncalf :: CardDef
mooncalf =
  (enemy "z-circus-ex-mortis-246" "Mooncalf" Set.LunaticNight 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Creature, Monster, Abomination]
      }

-- new_moon_daredevils
newMoonAcrobat :: CardDef
newMoonAcrobat =
  (enemy "z-circus-ex-mortis-248" "New Moon Acrobat" Set.NewMoonDaredevils 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonBeastTamer :: CardDef
newMoonBeastTamer =
  (enemy "z-circus-ex-mortis-249" "New Moon Beast Tamer" Set.NewMoonDaredevils 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonStrongman :: CardDef
newMoonStrongman =
  (enemy "z-circus-ex-mortis-250" "New Moon Strongman" Set.NewMoonDaredevils 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 1
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

-- new_moon_entertainers
newMoonCarny :: CardDef
newMoonCarny =
  (enemy "z-circus-ex-mortis-252" "New Moon Carny" Set.NewMoonEntertainers 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonClown :: CardDef
newMoonClown =
  (enemy "z-circus-ex-mortis-253" "New Moon Clown" Set.NewMoonEntertainers 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }

newMoonMagician :: CardDef
newMoonMagician =
  (enemy "z-circus-ex-mortis-254" "New Moon Magician" Set.NewMoonEntertainers 1)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Humanoid, Performer]
      }
