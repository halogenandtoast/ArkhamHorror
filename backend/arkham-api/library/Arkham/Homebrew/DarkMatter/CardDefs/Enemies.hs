module Arkham.Homebrew.DarkMatter.CardDefs.Enemies where

import Arkham.Enemy.CardDefs.Import
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Keyword qualified as Keyword
import Arkham.LocationSymbol qualified as LS

withScanIcons :: [LS.LocationSymbol] -> CardDef -> CardDef
withScanIcons icons def = def {cdMeta = insertMap "scanIcons" (toJSON icons) def.meta}

-- deep_space
theFeasterFromAfar :: CardDef
theFeasterFromAfar =
  (enemy "z-dark-matter-006" "The Feaster from Afar" Set.DeepSpace 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Avatar, AncientOne, Elite]
      , cdVictoryPoints = Just 1
      }

-- the_tatterdemalion
cybervirus :: CardDef
cybervirus =
  withScanIcons [LS.Trefoil]
    $ (enemy "z-dark-matter-028" "Cybervirus" Set.TheTatterdemalion 1)
        { cdSanityDamage = sanityDamage 2
        , cdFight = fight 2
        , cdEvade = evade 4
        , cdHealth = healthPerInvestigator 2
        , cdCardTraits = setFromList [Virtual]
        , cdVictoryPoints = Just 1
        , cdKeywords = setFromList [Keyword.Peril, Keyword.Hidden]
        }

jv7Hyades :: CardDef
jv7Hyades =
  withScanIcons [LS.T]
    $ (enemy "z-dark-matter-033" ("JV-7 'Hyades'" <:> "Artificial Co-Pilot") Set.TheTatterdemalion 1)
        { cdHealthDamage = healthDamage 1
        , cdSanityDamage = sanityDamage 1
        , cdFight = fight 2
        , cdEvade = evade 2
        , cdHealth = health 3
        , cdCardTraits = setFromList [AI, Machine]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Alert]
        }

lr02Hali :: CardDef
lr02Hali =
  withScanIcons [LS.Triangle, LS.Circle, LS.Plus]
    $ (enemy "z-dark-matter-035" "LR-02 'Hali'" Set.TheTatterdemalion 1)
        { cdHealthDamage = healthDamage 1
        , cdSanityDamage = sanityDamage 1
        , cdFight = fight 3
        , cdEvade = evade 4
        , cdHealth = healthPerInvestigator 2
        , cdCardTraits = setFromList [Humanoid, AI, Machine]
        , cdKeywords = setFromList [Keyword.Aloof, Keyword.Retaliate]
        , cdVictoryPoints = Just 1
        }

-- Back of agenda z-dark-matter-015 ("The Ghost Ship"); spawned when that agenda
-- advances (see the scenario/agenda modules).
uplA21Demhe :: CardDef
uplA21Demhe =
  (enemy "z-dark-matter-015b" ("UPL-A21 'Demhe'" <:> "Unmanned Power Loader") Set.TheTatterdemalion 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [AI, Machine, Elite]
      , cdKeywords = setFromList [Keyword.Hunter]
      , cdVictoryPoints = Just 1
      }

-- artificial_intelligence
systemBug :: CardDef
systemBug =
  (enemy "z-dark-matter-052" "System Bug" Set.ArtificialIntelligence 3)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [AI, Spider, Machine]
      }

-- electric_nightmare
shadowOfThoughts :: CardDef
shadowOfThoughts =
  (enemy "z-dark-matter-079" "Shadow of Thoughts" Set.ElectricNightmare 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Virtual, Abomination]
      , cdVictoryPoints = Just 1
      }

glitchInTheSystem :: CardDef
glitchInTheSystem =
  (enemy "z-dark-matter-082" "Glitch in the System" Set.ElectricNightmare 3)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 0
      , cdHealth = health 2
      , cdCardTraits = setFromList [Virtual]
      }

manifestedWhispers :: CardDef
manifestedWhispers =
  (enemy "z-dark-matter-083" "Manifested Whispers" Set.ElectricNightmare 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster]
      }

virtualByakhee :: CardDef
virtualByakhee =
  (enemy "z-dark-matter-085" "Virtual Byakhee" Set.ElectricNightmare 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Virtual, Monster, Byakhee]
      }

-- the_boogeyman
theBOOGEYMAN :: CardDef
theBOOGEYMAN =
  (enemy "z-dark-matter-086" ("THE BOOGEYMAN" <:> "Virtual Nightmare") Set.TheBoogeyman 1)
      { cdSanityDamage = sanityDamage 2
      , cdCardTraits = setFromList [Virtual, Monster, Elite]
      }

-- lost_quantum
houndOfTindalos :: CardDef
houndOfTindalos =
  (enemy "z-dark-matter-108" "Hound of Tindalos" Set.LostQuantum 3)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Creature, Liminal, Elite]
      , cdVictoryPoints = Just 1
      }

miGoStabilizer :: CardDef
miGoStabilizer =
  (enemy "z-dark-matter-110" "Mi-Go Stabilizer" Set.LostQuantum 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 1
      , cdHealth = health 2
      , cdCardTraits = setFromList [MiGo, Machine]
      }

quantumPhantom :: CardDef
quantumPhantom =
  (enemy "z-dark-matter-113" "Quantum Phantom" Set.LostQuantum 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Quantum, Geist]
      }

-- in_the_shadow_of_earth
theEntity :: CardDef
theEntity =
  (enemy "z-dark-matter-124" "The Entity" Set.InTheShadowOfEarth 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Abomination, Elite]
      , cdVictoryPoints = Just 2
      }

corruptedMachine :: CardDef
corruptedMachine =
  (enemy "z-dark-matter-147" "Corrupted Machine" Set.InTheShadowOfEarth 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Machine]
      }

mimic :: CardDef
mimic =
  (enemy "z-dark-matter-151" "Mimic" Set.InTheShadowOfEarth 3)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster]
      }

rats :: CardDef
rats =
  (enemy "z-dark-matter-154" "Rats?" Set.InTheShadowOfEarth 3)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster, Creature]
      }

-- strange_moons
theGreys :: CardDef
theGreys =
  doubleSided "z-dark-matter-166b"
      $ (enemy "z-dark-matter-166" "The Greys" Set.StrangeMoons 1)
        { cdSanityDamage = sanityDamage 1
        , cdFight = fight 4
        , cdEvade = evade 4
        , cdHealth = health 1
        , cdCardTraits = setFromList [Alien, Humanoid]
        }

parasite :: CardDef
parasite =
  (enemy "z-dark-matter-187" "Parasite" Set.StrangeMoons 3)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster]
      }

-- interstellar_predators
stalkingByakhee :: CardDef
stalkingByakhee =
  (enemy "z-dark-matter-191" "Stalking Byakhee" Set.InterstellarPredators 3)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Byakhee]
      }

viciousByakhee :: CardDef
viciousByakhee =
  (enemy "z-dark-matter-192" "Vicious Byakhee" Set.InterstellarPredators 3)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Byakhee]
      }

-- the_machine_in_yellow
yourOtherSelf :: CardDef
yourOtherSelf =
  (enemy "z-dark-matter-200" "Your Other Self" Set.TheMachineInYellow 4)
      { cdSanityDamage = sanityDamage 1
      , cdCardTraits = setFromList [Virtual, Humanoid, Elite]
      }

daemonOfNis :: CardDef
daemonOfNis =
  (enemy "z-dark-matter-201" ("Daemon of Nis" <:> "Devourer of Memories") Set.TheMachineInYellow 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Abomination, Monster]
      , cdVictoryPoints = Just 1
      }

spiritOfThan :: CardDef
spiritOfThan =
  (enemy "z-dark-matter-211" ("Spirit of Than" <:> "The One that Got Away") Set.TheMachineInYellow 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Abomination, Geist]
      , cdVictoryPoints = Just 1
      }

-- fragment_of_carcosa
caveDweller :: CardDef
caveDweller =
  (enemy "z-dark-matter-233" ("Cave Dweller" <:> "Suspiciously Familiar") Set.FragmentOfCarcosa 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 4
      , cdEvade = evade 6
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Cultist, Humanoid]
      , cdVictoryPoints = Just 1
      }

sophisticSpires :: CardDef
sophisticSpires =
  (enemy "z-dark-matter-242" "Sophistic Spires" Set.FragmentOfCarcosa 2)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 5
      , cdHealth = health 5
      , cdCardTraits = setFromList [Obstacle]
      }

tatteredCurtains :: CardDef
tatteredCurtains =
  (enemy "z-dark-matter-243" "Tattered Curtains" Set.FragmentOfCarcosa 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Obstacle]
      }

yellowMists :: CardDef
yellowMists =
  (enemy "z-dark-matter-245" "Yellow Mists" Set.FragmentOfCarcosa 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Obstacle]
      , cdVictoryPoints = Just 1
      }

-- starfall
tassilda :: CardDef
tassilda =
  (enemy "z-dark-matter-257" ("Tassilda" <:> "Royal Princess of Carcosa") Set.Starfall 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 6
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      }

yithianGuard :: CardDef
yithianGuard =
  (enemy "z-dark-matter-259" "Yithian Guard" Set.Starfall 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster, Yithian]
      , cdVictoryPoints = Just 1
      }

miGoSentinel :: CardDef
miGoSentinel =
  (enemy "z-dark-matter-260" "Mi-Go Sentinel" Set.Starfall 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster, MiGo]
      , cdVictoryPoints = Just 1
      }

domaagTeel :: CardDef
domaagTeel =
  (enemy "z-dark-matter-261" ("Domaag T’eel" <:> "The Mistake that Ended the World") Set.Starfall 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [AncientOne, Abomination, Elite]
      , cdVictoryPoints = Just 1
      }

shamblerFromTheStars :: CardDef
shamblerFromTheStars =
  (enemy "z-dark-matter-276" "Shambler from the Stars" Set.Starfall 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster, Elite]
      , cdVictoryPoints = Just 1
      }

exoroid :: CardDef
exoroid =
  (enemy "z-dark-matter-277" "Exoroid" Set.Starfall 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster]
      }

martianCrab :: CardDef
martianCrab =
  (enemy "z-dark-matter-280" "Martian Crab" Set.Starfall 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Creature]
      , cdVictoryPoints = Just 1
      }

cyberCultist :: CardDef
cyberCultist =
  (enemy "z-dark-matter-286" "Cyber-Cultist" Set.Starfall 2)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 1
      , cdHealth = health 1
      , cdCardTraits = setFromList [Cultist, Humanoid]
      }

spacePirates :: CardDef
spacePirates =
  (enemy "z-dark-matter-289" "Space Pirates" Set.Starfall 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Criminal]
      }

voidByakhee :: CardDef
voidByakhee =
  (enemy "z-dark-matter-291" "Void Byakhee" Set.Starfall 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Monster, Byakhee]
      }
