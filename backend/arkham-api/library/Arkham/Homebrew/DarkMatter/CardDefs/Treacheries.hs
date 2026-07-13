module Arkham.Homebrew.DarkMatter.CardDefs.Treacheries where

import Arkham.Treachery.CardDefs.Import
import Arkham.Homebrew.DarkMatter.Sets qualified as Set

-- anachronism
anachronism :: CardDef
anachronism =
  (treachery "z-dark-matter-001" "Anachronism" Set.Anachronism 2) {cdCardTraits = setFromList [Madness, Paradox]}

-- dark_past
hauntingPast :: CardDef
hauntingPast =
  (treachery "z-dark-matter-002" "Haunting Past" Set.DarkPast 2) {cdCardTraits = setFromList [Scheme]}

reminiscencePledge :: CardDef
reminiscencePledge =
  (treachery "z-dark-matter-003" "Reminiscence (Pledge)" Set.DarkPast 1) {cdCardTraits = setFromList [Pact]}

reminiscenceSecrets :: CardDef
reminiscenceSecrets =
  (treachery "z-dark-matter-004" "Reminiscence (Secrets)" Set.DarkPast 1) {cdCardTraits = setFromList [Pact]}

reminiscenceCovenant :: CardDef
reminiscenceCovenant =
  (treachery "z-dark-matter-005" "Reminiscence (Covenant)" Set.DarkPast 1) {cdCardTraits = setFromList [Pact]}

-- deep_space
callOfTheVoid :: CardDef
callOfTheVoid =
  (treachery "z-dark-matter-007" "Call of the Void" Set.DeepSpace 1) {cdCardTraits = setFromList [Omen]}

coldVacuum :: CardDef
coldVacuum =
  (treachery "z-dark-matter-008" "Cold Vacuum" Set.DeepSpace 2) {cdCardTraits = setFromList [Hazard]}

micrometeoroid :: CardDef
micrometeoroid =
  (treachery "z-dark-matter-009" "Micrometeoroid" Set.DeepSpace 2) {cdCardTraits = setFromList [Hazard]}

theColorsOfSpace :: CardDef
theColorsOfSpace =
  (treachery "z-dark-matter-010" "The Colors of Space" Set.DeepSpace 2) {cdCardTraits = setFromList [Hazard]}

-- endtimes
grimFuture :: CardDef
grimFuture =
  (treachery "z-dark-matter-011" "Grim Future" Set.Endtimes 2) {cdCardTraits = setFromList [Endtimes]}

futureEvils :: CardDef
futureEvils =
  (treachery "z-dark-matter-012" "Future Evils" Set.Endtimes 3) {cdCardTraits = setFromList [Omen, Endtimes]}

-- the_tatterdemalion
artificialGravityMalfunction :: CardDef
artificialGravityMalfunction =
  (treachery "z-dark-matter-043" "Artificial Gravity Malfunction" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

cabinPressure :: CardDef
cabinPressure =
  (treachery "z-dark-matter-044" "Cabin Pressure" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Madness]}

coolantLeak :: CardDef
coolantLeak =
  (treachery "z-dark-matter-045" "Coolant Leak" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

decompression :: CardDef
decompression =
  (treachery "z-dark-matter-046" "Decompression" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

highRadiationLevels :: CardDef
highRadiationLevels =
  (treachery "z-dark-matter-047" "High Radiation Levels" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

-- artificial_intelligence
allSeeingEye :: CardDef
allSeeingEye =
  (treachery "z-dark-matter-048" "All-Seeing Eye" Set.ArtificialIntelligence 3) {cdCardTraits = setFromList [AI]}

electricSurge :: CardDef
electricSurge =
  (treachery "z-dark-matter-049" "Electric Surge" Set.ArtificialIntelligence 2) {cdCardTraits = setFromList [Hazard]}

hallucinatoryHolograms :: CardDef
hallucinatoryHolograms =
  (treachery "z-dark-matter-050" "Hallucinatory Holograms" Set.ArtificialIntelligence 2) {cdCardTraits = setFromList [Terror]}

predictiveAlgorithm :: CardDef
predictiveAlgorithm =
  (treachery "z-dark-matter-051" "Predictive Algorithm" Set.ArtificialIntelligence 2) {cdCardTraits = setFromList [AI, Scheme]}

-- electric_nightmare
desync :: CardDef
desync =
  (weakness "z-dark-matter-066" "Desync")
      { cdCardTraits = setFromList [Madness]
      , cdEncounterSet = Just Set.ElectricNightmare
      , cdEncounterSetQuantity = Just 4
      }

digitalCorrosion :: CardDef
digitalCorrosion =
  (treachery "z-dark-matter-078" "Digital Corrosion" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Virtual, Hazard]}

decoherence :: CardDef
decoherence =
  (treachery "z-dark-matter-080" "Decoherence" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Virtual, Terror]}

duplication :: CardDef
duplication =
  (treachery "z-dark-matter-081" "Duplication" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Scheme]}

nonEuclideanGeometry :: CardDef
nonEuclideanGeometry =
  (treachery "z-dark-matter-084" "Non-Euclidean Geometry" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Hazard]}

-- the_boogeyman
comeCLOSER :: CardDef
comeCLOSER =
  (treachery "z-dark-matter-087" "COME CLOSER" Set.TheBoogeyman 2) {cdCardTraits = setFromList [Nightmare, Terror]}

rememberME :: CardDef
rememberME =
  (treachery "z-dark-matter-088" "REMEMBER ME?" Set.TheBoogeyman 2) {cdCardTraits = setFromList [Nightmare, Terror]}

surprise :: CardDef
surprise =
  (treachery "z-dark-matter-089" "SURPRISE!" Set.TheBoogeyman 2) {cdCardTraits = setFromList [Virtual, Nightmare]}

-- lost_quantum
entangled :: CardDef
entangled =
  (treachery "z-dark-matter-107" "Entangled" Set.LostQuantum 3) {cdCardTraits = setFromList [Quantum]}

incomprehensible :: CardDef
incomprehensible =
  (treachery "z-dark-matter-109" "Incomprehensible" Set.LostQuantum 2) {cdCardTraits = setFromList [Paradox]}

paradoxicalThreat :: CardDef
paradoxicalThreat =
  (treachery "z-dark-matter-111" "Paradoxical Threat" Set.LostQuantum 2) {cdCardTraits = setFromList [Omen]}

quantumCollapse :: CardDef
quantumCollapse =
  (treachery "z-dark-matter-112" "Quantum Collapse" Set.LostQuantum 3) {cdCardTraits = setFromList [Quantum]}

radioactiveDecay :: CardDef
radioactiveDecay =
  (treachery "z-dark-matter-114" "Radioactive Decay" Set.LostQuantum 2) {cdCardTraits = setFromList [Hazard]}

-- in_the_shadow_of_earth
anothersWoe :: CardDef
anothersWoe =
  (treachery "z-dark-matter-145" "Another's Woe" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Omen]}

contamination :: CardDef
contamination =
  (treachery "z-dark-matter-146" "Contamination" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Injury, Madness]}

fromTheDark :: CardDef
fromTheDark =
  (treachery "z-dark-matter-148" "From the Dark" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Tactic]}

hopeless :: CardDef
hopeless =
  (treachery "z-dark-matter-149" "Hopeless" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Terror]}

infection :: CardDef
infection =
  (treachery "z-dark-matter-150" "Infection" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Injury, Curse]}

paleBlueDot :: CardDef
paleBlueDot =
  (treachery "z-dark-matter-152" "Pale Blue Dot" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Terror]}

perfectImitation :: CardDef
perfectImitation =
  (treachery "z-dark-matter-153" "Perfect Imitation" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Trap]}

scrambled :: CardDef
scrambled =
  (treachery "z-dark-matter-155" "Scrambled" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Blunder]}

-- strange_moons
alienAid :: CardDef
alienAid =
  (treachery "z-dark-matter-182" "Alien Aid" Set.StrangeMoons 2) {cdCardTraits = setFromList [MiGo, Pact]}

closeEncounters :: CardDef
closeEncounters =
  (treachery "z-dark-matter-183" "Close Encounters" Set.StrangeMoons 2) {cdCardTraits = setFromList [Hazard]}

innocentMishap :: CardDef
innocentMishap =
  (treachery "z-dark-matter-184" "Innocent Mishap" Set.StrangeMoons 2) {cdCardTraits = setFromList [Blunder]}

lostInTranslation :: CardDef
lostInTranslation =
  (treachery "z-dark-matter-185" "Lost in Translation" Set.StrangeMoons 2) {cdCardTraits = setFromList [Blunder]}

miGoExperiments :: CardDef
miGoExperiments =
  (treachery "z-dark-matter-186" "Mi-Go Experiments" Set.StrangeMoons 2) {cdCardTraits = setFromList [MiGo, Terror]}

simulationDiscrepancy :: CardDef
simulationDiscrepancy =
  (treachery "z-dark-matter-188" "Simulation Discrepancy" Set.StrangeMoons 2) {cdCardTraits = setFromList [Blunder, Obstacle]}

toxicPits :: CardDef
toxicPits =
  (treachery "z-dark-matter-189" "Toxic Pits" Set.StrangeMoons 2) {cdCardTraits = setFromList [Hazard]}

-- interstellar_predators
extraterrestrialAssault :: CardDef
extraterrestrialAssault =
  (treachery "z-dark-matter-190" "Extraterrestrial Assault" Set.InterstellarPredators 2) {cdCardTraits = setFromList [Pact]}

-- the_machine_in_yellow
darkReflectionsMalingerer :: CardDef
darkReflectionsMalingerer =
  (treachery "z-dark-matter-202" "Dark Reflections (Malingerer)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsMurderer :: CardDef
darkReflectionsMurderer =
  (treachery "z-dark-matter-203" "Dark Reflections (Murderer)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsSycophant :: CardDef
darkReflectionsSycophant =
  (treachery "z-dark-matter-204" "Dark Reflections (Sycophant)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsZealot :: CardDef
darkReflectionsZealot =
  (treachery "z-dark-matter-205" "Dark Reflections (Zealot)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

delusionalMadness :: CardDef
delusionalMadness =
  (treachery "z-dark-matter-206" "Delusional Madness" Set.TheMachineInYellow 3) {cdCardTraits = setFromList [Madness]}

fathomlessRegrets :: CardDef
fathomlessRegrets =
  (treachery "z-dark-matter-207" "Fathomless Regrets" Set.TheMachineInYellow 2) {cdCardTraits = setFromList [Terror]}

forbiddingPromises :: CardDef
forbiddingPromises =
  (treachery "z-dark-matter-208" "Forbidding Promises" Set.TheMachineInYellow 2) {cdCardTraits = setFromList [Terror]}

persistenceOfMemory :: CardDef
persistenceOfMemory =
  (treachery "z-dark-matter-209" "Persistence of Memory" Set.TheMachineInYellow 2)

perspectiveSwitch :: CardDef
perspectiveSwitch =
  (treachery "z-dark-matter-210" "Perspective Switch" Set.TheMachineInYellow 2) {cdCardTraits = setFromList [Madness]}

-- fragment_of_carcosa
brokenReality :: CardDef
brokenReality =
  (treachery "z-dark-matter-231" "Broken Reality" Set.FragmentOfCarcosa 3) {cdCardTraits = setFromList [Hazard]}

caveCollapse :: CardDef
caveCollapse =
  (treachery "z-dark-matter-232" "Cave Collapse" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hazard]}

chosenByHim :: CardDef
chosenByHim =
  (treachery "z-dark-matter-234" "Chosen by Him" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

echoesOfTassildaMatter :: CardDef
echoesOfTassildaMatter =
  (treachery "z-dark-matter-235" "Echoes of Tassilda (Matter)" Set.FragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 1
      }

echoesOfTassildaMind :: CardDef
echoesOfTassildaMind =
  (treachery "z-dark-matter-236" "Echoes of Tassilda (Mind)" Set.FragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 1
      }

hastursDomain :: CardDef
hastursDomain =
  (treachery "z-dark-matter-237" "Hastur's Domain" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Terror]}

irresistibleTruths :: CardDef
irresistibleTruths =
  (treachery "z-dark-matter-238" "Irresistible Truths" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

madnessOfCarcosa :: CardDef
madnessOfCarcosa =
  (treachery "z-dark-matter-239" "Madness of Carcosa" Set.FragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 2
      }

sceneShifting :: CardDef
sceneShifting =
  (treachery "z-dark-matter-240" "Scene Shifting" Set.FragmentOfCarcosa 3) {cdCardTraits = setFromList [Omen]}

songOfYourSoul :: CardDef
songOfYourSoul =
  (treachery "z-dark-matter-241" "Song of Your Soul" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

unstableDimension :: CardDef
unstableDimension =
  (treachery "z-dark-matter-244" "Unstable Dimension" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hazard]}

-- starfall
alienation :: CardDef
alienation =
  (treachery "z-dark-matter-285" "Alienation" Set.Starfall 2) {cdCardTraits = setFromList [Terror]}

solarEclipse :: CardDef
solarEclipse =
  (treachery "z-dark-matter-287" "Solar Eclipse" Set.Starfall 2) {cdCardTraits = setFromList [Power]}

theDarkForest :: CardDef
theDarkForest =
  (treachery "z-dark-matter-288" "The Dark Forest" Set.Starfall 2) {cdCardTraits = setFromList [Omen, Endtimes]}

theStarsWereRight :: CardDef
theStarsWereRight =
  (treachery "z-dark-matter-290" "The Stars Were Right" Set.Starfall 2) {cdCardTraits = setFromList [Endtimes, Power]}

-- hastur_s_gaze
solarFlare :: CardDef
solarFlare =
  (treachery "z-dark-matter-292" "Solar Flare" Set.HastursGaze 3) {cdCardTraits = setFromList [Hazard]}

radiantCrown :: CardDef
radiantCrown =
  (treachery "z-dark-matter-293" "Radiant Crown" Set.HastursGaze 2) {cdCardTraits = setFromList [Power]}
