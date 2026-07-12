module Arkham.Enemy.CardDefs.DarkMatter where

import Arkham.Enemy.CardDefs.Import

-- deep_space
theFeasterFromAfarDarkMatter :: CardDef
theFeasterFromAfarDarkMatter =
  (enemy "z-dark-matter-006" "The Feaster from Afar" DarkMatterDeepSpace 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Avatar, AncientOne, Elite]
      , cdVictoryPoints = Just 1
      }

-- the_tatterdemalion
cybervirusDarkMatter :: CardDef
cybervirusDarkMatter =
  (enemy "z-dark-matter-028" "Cybervirus" DarkMatterTheTatterdemalion 1)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Virtual]
      , cdVictoryPoints = Just 1
      }

jv7HyadesDarkMatter :: CardDef
jv7HyadesDarkMatter =
  (enemy "z-dark-matter-033" ("JV-7 'Hyades'" <:> "Artificial Co-Pilot") DarkMatterTheTatterdemalion 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 2
      , cdHealth = health 3
      , cdCardTraits = setFromList [AI, Machine]
      }

lr02HaliDarkMatter :: CardDef
lr02HaliDarkMatter =
  (enemy "z-dark-matter-035" "LR-02 'Hali'" DarkMatterTheTatterdemalion 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Humanoid, AI, Machine]
      , cdVictoryPoints = Just 1
      }

-- artificial_intelligence
systemBugDarkMatter :: CardDef
systemBugDarkMatter =
  (enemy "z-dark-matter-052" "System Bug" DarkMatterArtificialIntelligence 3)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [AI, Spider, Machine]
      }

-- electric_nightmare
shadowOfThoughtsDarkMatter :: CardDef
shadowOfThoughtsDarkMatter =
  (enemy "z-dark-matter-079" "Shadow of Thoughts" DarkMatterElectricNightmare 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdEvade = evade 2
      , cdHealth = health 5
      , cdCardTraits = setFromList [Virtual, Abomination]
      , cdVictoryPoints = Just 1
      }

glitchInTheSystemDarkMatter :: CardDef
glitchInTheSystemDarkMatter =
  (enemy "z-dark-matter-082" "Glitch in the System" DarkMatterElectricNightmare 3)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 0
      , cdHealth = health 2
      , cdCardTraits = setFromList [Virtual]
      }

manifestedWhispersDarkMatter :: CardDef
manifestedWhispersDarkMatter =
  (enemy "z-dark-matter-083" "Manifested Whispers" DarkMatterElectricNightmare 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster]
      }

virtualByakheeDarkMatter :: CardDef
virtualByakheeDarkMatter =
  (enemy "z-dark-matter-085" "Virtual Byakhee" DarkMatterElectricNightmare 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Virtual, Monster, Byakhee]
      }

-- the_boogeyman
theBOOGEYMANDarkMatter :: CardDef
theBOOGEYMANDarkMatter =
  (enemy "z-dark-matter-086" ("THE BOOGEYMAN" <:> "Virtual Nightmare") DarkMatterTheBoogeyman 1)
      { cdSanityDamage = sanityDamage 2
      , cdCardTraits = setFromList [Virtual, Monster, Elite]
      }

-- lost_quantum
houndOfTindalosDarkMatter :: CardDef
houndOfTindalosDarkMatter =
  (enemy "z-dark-matter-108" "Hound of Tindalos" DarkMatterLostQuantum 3)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 4
      , cdHealth = health 4
      , cdCardTraits = setFromList [Creature, Liminal, Elite]
      , cdVictoryPoints = Just 1
      }

miGoStabilizerDarkMatter :: CardDef
miGoStabilizerDarkMatter =
  (enemy "z-dark-matter-110" "Mi-Go Stabilizer" DarkMatterLostQuantum 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 1
      , cdHealth = health 2
      , cdCardTraits = setFromList [MiGo, Machine]
      }

quantumPhantomDarkMatter :: CardDef
quantumPhantomDarkMatter =
  (enemy "z-dark-matter-113" "Quantum Phantom" DarkMatterLostQuantum 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Quantum, Geist]
      }

-- in_the_shadow_of_earth
theEntityDarkMatter :: CardDef
theEntityDarkMatter =
  (enemy "z-dark-matter-124" "The Entity" DarkMatterInTheShadowOfEarth 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = health 3
      , cdCardTraits = setFromList [Abomination, Elite]
      , cdVictoryPoints = Just 2
      }

corruptedMachineDarkMatter :: CardDef
corruptedMachineDarkMatter =
  (enemy "z-dark-matter-147" "Corrupted Machine" DarkMatterInTheShadowOfEarth 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 2
      , cdCardTraits = setFromList [Machine]
      }

mimicDarkMatter :: CardDef
mimicDarkMatter =
  (enemy "z-dark-matter-151" "Mimic" DarkMatterInTheShadowOfEarth 3)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster]
      }

ratsDarkMatter :: CardDef
ratsDarkMatter =
  (enemy "z-dark-matter-154" "Rats?" DarkMatterInTheShadowOfEarth 3)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 1
      , cdEvade = evade 3
      , cdHealth = health 1
      , cdCardTraits = setFromList [Monster, Creature]
      }

-- strange_moons
theGreysDarkMatter :: CardDef
theGreysDarkMatter =
  doubleSided "z-dark-matter-166b"
      $ (enemy "z-dark-matter-166" "The Greys" DarkMatterStrangeMoons 1)
        { cdSanityDamage = sanityDamage 1
        , cdFight = fight 4
        , cdEvade = evade 4
        , cdHealth = health 1
        , cdCardTraits = setFromList [Alien, Humanoid]
        }

parasiteDarkMatter :: CardDef
parasiteDarkMatter =
  (enemy "z-dark-matter-187" "Parasite" DarkMatterStrangeMoons 3)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster]
      }

-- interstellar_predators
stalkingByakheeDarkMatter :: CardDef
stalkingByakheeDarkMatter =
  (enemy "z-dark-matter-191" "Stalking Byakhee" DarkMatterInterstellarPredators 3)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = health 2
      , cdCardTraits = setFromList [Monster, Byakhee]
      }

viciousByakheeDarkMatter :: CardDef
viciousByakheeDarkMatter =
  (enemy "z-dark-matter-192" "Vicious Byakhee" DarkMatterInterstellarPredators 3)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Monster, Byakhee]
      }

-- the_machine_in_yellow
yourOtherSelfDarkMatter :: CardDef
yourOtherSelfDarkMatter =
  (enemy "z-dark-matter-200" "Your Other Self" DarkMatterTheMachineInYellow 4)
      { cdSanityDamage = sanityDamage 1
      , cdCardTraits = setFromList [Virtual, Humanoid, Elite]
      }

daemonOfNisDarkMatter :: CardDef
daemonOfNisDarkMatter =
  (enemy "z-dark-matter-201" ("Daemon of Nis" <:> "Devourer of Memories") DarkMatterTheMachineInYellow 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Abomination, Monster]
      , cdVictoryPoints = Just 1
      }

spiritOfThanDarkMatter :: CardDef
spiritOfThanDarkMatter =
  (enemy "z-dark-matter-211" ("Spirit of Than" <:> "The One that Got Away") DarkMatterTheMachineInYellow 1)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 2
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Abomination, Geist]
      , cdVictoryPoints = Just 1
      }

-- fragment_of_carcosa
caveDwellerDarkMatter :: CardDef
caveDwellerDarkMatter =
  (enemy "z-dark-matter-233" ("Cave Dweller" <:> "Suspiciously Familiar") DarkMatterFragmentOfCarcosa 1)
      { cdHealthDamage = healthDamage 3
      , cdFight = fight 4
      , cdEvade = evade 6
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [Monster, Cultist, Humanoid]
      , cdVictoryPoints = Just 1
      }

sophisticSpiresDarkMatter :: CardDef
sophisticSpiresDarkMatter =
  (enemy "z-dark-matter-242" "Sophistic Spires" DarkMatterFragmentOfCarcosa 2)
      { cdSanityDamage = sanityDamage 2
      , cdFight = fight 2
      , cdEvade = evade 5
      , cdHealth = health 5
      , cdCardTraits = setFromList [Obstacle]
      }

tatteredCurtainsDarkMatter :: CardDef
tatteredCurtainsDarkMatter =
  (enemy "z-dark-matter-243" "Tattered Curtains" DarkMatterFragmentOfCarcosa 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 2
      , cdHealth = health 4
      , cdCardTraits = setFromList [Obstacle]
      }

yellowMistsDarkMatter :: CardDef
yellowMistsDarkMatter =
  (enemy "z-dark-matter-245" "Yellow Mists" DarkMatterFragmentOfCarcosa 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 1
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Obstacle]
      , cdVictoryPoints = Just 1
      }

-- starfall
tassildaDarkMatter :: CardDef
tassildaDarkMatter =
  (enemy "z-dark-matter-257" ("Tassilda" <:> "Royal Princess of Carcosa") DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 3
      , cdSanityDamage = sanityDamage 3
      , cdFight = fight 6
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 4
      , cdCardTraits = setFromList [AncientOne, Elite]
      }

yithianGuardDarkMatter :: CardDef
yithianGuardDarkMatter =
  (enemy "z-dark-matter-259" "Yithian Guard" DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster, Yithian]
      , cdVictoryPoints = Just 1
      }

miGoSentinelDarkMatter :: CardDef
miGoSentinelDarkMatter =
  (enemy "z-dark-matter-260" "Mi-Go Sentinel" DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 4
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster, MiGo]
      , cdVictoryPoints = Just 1
      }

domaagTeelDarkMatter :: CardDef
domaagTeelDarkMatter =
  (enemy "z-dark-matter-261" ("Domaag T’eel" <:> "The Mistake that Ended the World") DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 2
      , cdSanityDamage = sanityDamage 2
      , cdFight = fight 5
      , cdEvade = evade 5
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [AncientOne, Abomination, Elite]
      , cdVictoryPoints = Just 1
      }

shamblerFromTheStarsDarkMatter :: CardDef
shamblerFromTheStarsDarkMatter =
  (enemy "z-dark-matter-276" "Shambler from the Stars" DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster, Elite]
      , cdVictoryPoints = Just 1
      }

exoroidDarkMatter :: CardDef
exoroidDarkMatter =
  (enemy "z-dark-matter-277" "Exoroid" DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 2
      , cdEvade = evade 4
      , cdHealth = healthPerInvestigator 2
      , cdCardTraits = setFromList [Monster]
      }

martianCrabDarkMatter :: CardDef
martianCrabDarkMatter =
  (enemy "z-dark-matter-280" "Martian Crab" DarkMatterStarfall 1)
      { cdHealthDamage = healthDamage 2
      , cdFight = fight 5
      , cdEvade = evade 2
      , cdHealth = healthPerInvestigator 3
      , cdCardTraits = setFromList [Creature]
      , cdVictoryPoints = Just 1
      }

cyberCultistDarkMatter :: CardDef
cyberCultistDarkMatter =
  (enemy "z-dark-matter-286" "Cyber-Cultist" DarkMatterStarfall 2)
      { cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 1
      , cdHealth = health 1
      , cdCardTraits = setFromList [Cultist, Humanoid]
      }

spacePiratesDarkMatter :: CardDef
spacePiratesDarkMatter =
  (enemy "z-dark-matter-289" "Space Pirates" DarkMatterStarfall 2)
      { cdHealthDamage = healthDamage 1
      , cdFight = fight 3
      , cdEvade = evade 3
      , cdHealth = health 4
      , cdCardTraits = setFromList [Humanoid, Criminal]
      }

voidByakheeDarkMatter :: CardDef
voidByakheeDarkMatter =
  (enemy "z-dark-matter-291" "Void Byakhee" DarkMatterStarfall 2)
      { cdHealthDamage = healthDamage 1
      , cdSanityDamage = sanityDamage 1
      , cdFight = fight 3
      , cdEvade = evade 4
      , cdHealth = health 3
      , cdCardTraits = setFromList [Monster, Byakhee]
      }
