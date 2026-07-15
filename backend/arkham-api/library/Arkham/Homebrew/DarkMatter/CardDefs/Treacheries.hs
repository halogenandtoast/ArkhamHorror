module Arkham.Homebrew.DarkMatter.CardDefs.Treacheries where

import Arkham.Treachery.CardDefs.Import
import Arkham.Homebrew.DarkMatter.Sets qualified as Set
import Arkham.Homebrew.DarkMatter.Traits

-- anachronism
anachronism :: CardDef
anachronism =
  (treachery ":dark-matter:001" "Anachronism" Set.Anachronism 2) {cdCardTraits = setFromList [Madness, Paradox]}

-- dark_past
hauntingPast :: CardDef
hauntingPast =
  (treachery ":dark-matter:002" "Haunting Past" Set.DarkPast 2) {cdCardTraits = setFromList [Scheme]}

reminiscencePledge :: CardDef
reminiscencePledge =
  (treachery ":dark-matter:003" "Reminiscence (Pledge)" Set.DarkPast 1) {cdCardTraits = setFromList [Pact]}

reminiscenceSecrets :: CardDef
reminiscenceSecrets =
  (treachery ":dark-matter:004" "Reminiscence (Secrets)" Set.DarkPast 1) {cdCardTraits = setFromList [Pact]}

reminiscenceCovenant :: CardDef
reminiscenceCovenant =
  (treachery ":dark-matter:005" "Reminiscence (Covenant)" Set.DarkPast 1) {cdCardTraits = setFromList [Pact]}

-- deep_space
callOfTheVoid :: CardDef
callOfTheVoid =
  (treachery ":dark-matter:008" "Call of the Void" Set.DeepSpace 1) {cdCardTraits = setFromList [Omen]}

coldVacuum :: CardDef
coldVacuum =
  (treachery ":dark-matter:009" "Cold Vacuum" Set.DeepSpace 2) {cdCardTraits = setFromList [Hazard]}

micrometeoroid :: CardDef
micrometeoroid =
  (treachery ":dark-matter:010" "Micrometeoroid" Set.DeepSpace 2) {cdCardTraits = setFromList [Hazard]}

theColorsOfSpace :: CardDef
theColorsOfSpace =
  (treachery ":dark-matter:011" "The Colors of Space" Set.DeepSpace 2) {cdCardTraits = setFromList [Hazard]}

-- endtimes
grimFuture :: CardDef
grimFuture =
  (treachery ":dark-matter:012" "Grim Future" Set.Endtimes 2) {cdCardTraits = setFromList [Endtimes]}

futureEvils :: CardDef
futureEvils =
  (treachery ":dark-matter:013" "Future Evils" Set.Endtimes 3) {cdCardTraits = setFromList [Omen, Endtimes]}

-- the_tatterdemalion
artificialGravityMalfunction :: CardDef
artificialGravityMalfunction =
  (treachery ":dark-matter:043" "Artificial Gravity Malfunction" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

cabinPressure :: CardDef
cabinPressure =
  (treachery ":dark-matter:044" "Cabin Pressure" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Madness]}

coolantLeak :: CardDef
coolantLeak =
  (treachery ":dark-matter:045" "Coolant Leak" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

decompression :: CardDef
decompression =
  (treachery ":dark-matter:046" "Decompression" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

highRadiationLevels :: CardDef
highRadiationLevels =
  (treachery ":dark-matter:047" "High Radiation Levels" Set.TheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

-- artificial_intelligence
allSeeingEye :: CardDef
allSeeingEye =
  (treachery ":dark-matter:048" "All-Seeing Eye" Set.ArtificialIntelligence 3) {cdCardTraits = setFromList [AI]}

electricSurge :: CardDef
electricSurge =
  (treachery ":dark-matter:049" "Electric Surge" Set.ArtificialIntelligence 2) {cdCardTraits = setFromList [Hazard]}

hallucinatoryHolograms :: CardDef
hallucinatoryHolograms =
  (treachery ":dark-matter:050" "Hallucinatory Holograms" Set.ArtificialIntelligence 2) {cdCardTraits = setFromList [Terror]}

predictiveAlgorithm :: CardDef
predictiveAlgorithm =
  (treachery ":dark-matter:051" "Predictive Algorithm" Set.ArtificialIntelligence 2) {cdCardTraits = setFromList [AI, Scheme]}

-- electric_nightmare
desync :: CardDef
desync =
  (weakness ":dark-matter:064" "Desync")
      { cdCardTraits = setFromList [Madness]
      , cdEncounterSet = Just Set.ElectricNightmare
      , cdEncounterSetQuantity = Just 4
      }

digitalCorrosion :: CardDef
digitalCorrosion =
  (treachery ":dark-matter:075" "Digital Corrosion" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Virtual, Hazard]}

decoherence :: CardDef
decoherence =
  (treachery ":dark-matter:077" "Decoherence" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Virtual, Terror]}

duplication :: CardDef
duplication =
  (treachery ":dark-matter:078" "Duplication" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Scheme]}

nonEuclideanGeometry :: CardDef
nonEuclideanGeometry =
  (treachery ":dark-matter:083" "Non-Euclidean Geometry" Set.ElectricNightmare 2) {cdCardTraits = setFromList [Hazard]}

-- the_boogeyman
comeCLOSER :: CardDef
comeCLOSER =
  (treachery ":dark-matter:086" "COME CLOSER" Set.TheBoogeyman 2) {cdCardTraits = setFromList [Nightmare, Terror]}

rememberME :: CardDef
rememberME =
  (treachery ":dark-matter:087" "REMEMBER ME?" Set.TheBoogeyman 2) {cdCardTraits = setFromList [Nightmare, Terror]}

surprise :: CardDef
surprise =
  (treachery ":dark-matter:088" "SURPRISE!" Set.TheBoogeyman 2) {cdCardTraits = setFromList [Virtual, Nightmare]}

-- lost_quantum
entangled :: CardDef
entangled =
  (treachery ":dark-matter:104" "Entangled" Set.LostQuantum 3) {cdCardTraits = setFromList [Quantum]}

incomprehensible :: CardDef
incomprehensible =
  (treachery ":dark-matter:106" "Incomprehensible" Set.LostQuantum 2) {cdCardTraits = setFromList [Paradox]}

paradoxicalThreat :: CardDef
paradoxicalThreat =
  (treachery ":dark-matter:108" "Paradoxical Threat" Set.LostQuantum 2) {cdCardTraits = setFromList [Omen]}

quantumCollapse :: CardDef
quantumCollapse =
  (treachery ":dark-matter:109" "Quantum Collapse" Set.LostQuantum 3) {cdCardTraits = setFromList [Quantum]}

radioactiveDecay :: CardDef
radioactiveDecay =
  (treachery ":dark-matter:111" "Radioactive Decay" Set.LostQuantum 2) {cdCardTraits = setFromList [Hazard]}

-- in_the_shadow_of_earth
anothersWoe :: CardDef
anothersWoe =
  (treachery ":dark-matter:142" "Another's Woe" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Omen]}

contamination :: CardDef
contamination =
  (treachery ":dark-matter:143" "Contamination" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Injury, Madness]}

fromTheDark :: CardDef
fromTheDark =
  (treachery ":dark-matter:145" "From the Dark" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Tactic]}

hopeless :: CardDef
hopeless =
  (treachery ":dark-matter:146" "Hopeless" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Terror]}

infection :: CardDef
infection =
  (treachery ":dark-matter:147" "Infection" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Injury, Curse]}

paleBlueDot :: CardDef
paleBlueDot =
  (treachery ":dark-matter:149" "Pale Blue Dot" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Terror]}

perfectImitation :: CardDef
perfectImitation =
  (treachery ":dark-matter:150" "Perfect Imitation" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Trap]}

scrambled :: CardDef
scrambled =
  (treachery ":dark-matter:152" "Scrambled" Set.InTheShadowOfEarth 2) {cdCardTraits = setFromList [Blunder]}

-- strange_moons
alienAid :: CardDef
alienAid =
  (treachery ":dark-matter:179" "Alien Aid" Set.StrangeMoons 2) {cdCardTraits = setFromList [MiGo, Pact]}

closeEncounters :: CardDef
closeEncounters =
  (treachery ":dark-matter:180" "Close Encounters" Set.StrangeMoons 2) {cdCardTraits = setFromList [Hazard]}

innocentMishap :: CardDef
innocentMishap =
  (treachery ":dark-matter:181" "Innocent Mishap" Set.StrangeMoons 2) {cdCardTraits = setFromList [Blunder]}

lostInTranslation :: CardDef
lostInTranslation =
  (treachery ":dark-matter:182" "Lost in Translation" Set.StrangeMoons 2) {cdCardTraits = setFromList [Blunder]}

miGoExperiments :: CardDef
miGoExperiments =
  (treachery ":dark-matter:183" "Mi-Go Experiments" Set.StrangeMoons 2) {cdCardTraits = setFromList [MiGo, Terror]}

simulationDiscrepancy :: CardDef
simulationDiscrepancy =
  (treachery ":dark-matter:185" "Simulation Discrepancy" Set.StrangeMoons 2) {cdCardTraits = setFromList [Blunder, Obstacle]}

toxicPits :: CardDef
toxicPits =
  (treachery ":dark-matter:186" "Toxic Pits" Set.StrangeMoons 2) {cdCardTraits = setFromList [Hazard]}

-- interstellar_predators
extraterrestrialAssault :: CardDef
extraterrestrialAssault =
  (treachery ":dark-matter:187" "Extraterrestrial Assault" Set.InterstellarPredators 2) {cdCardTraits = setFromList [Pact]}

-- the_machine_in_yellow
darkReflectionsMalingerer :: CardDef
darkReflectionsMalingerer =
  (treachery ":dark-matter:199" "Dark Reflections (Malingerer)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsMurderer :: CardDef
darkReflectionsMurderer =
  (treachery ":dark-matter:200" "Dark Reflections (Murderer)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsSycophant :: CardDef
darkReflectionsSycophant =
  (treachery ":dark-matter:201" "Dark Reflections (Sycophant)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsZealot :: CardDef
darkReflectionsZealot =
  (treachery ":dark-matter:202" "Dark Reflections (Zealot)" Set.TheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

delusionalMadness :: CardDef
delusionalMadness =
  (treachery ":dark-matter:203" "Delusional Madness" Set.TheMachineInYellow 3) {cdCardTraits = setFromList [Madness]}

fathomlessRegrets :: CardDef
fathomlessRegrets =
  (treachery ":dark-matter:204" "Fathomless Regrets" Set.TheMachineInYellow 2) {cdCardTraits = setFromList [Terror]}

forbiddingPromises :: CardDef
forbiddingPromises =
  (treachery ":dark-matter:205" "Forbidding Promises" Set.TheMachineInYellow 2) {cdCardTraits = setFromList [Terror]}

persistenceOfMemory :: CardDef
persistenceOfMemory =
  (treachery ":dark-matter:206" "Persistence of Memory" Set.TheMachineInYellow 2)

perspectiveSwitch :: CardDef
perspectiveSwitch =
  (treachery ":dark-matter:207" "Perspective Switch" Set.TheMachineInYellow 2) {cdCardTraits = setFromList [Madness]}

-- fragment_of_carcosa
brokenReality :: CardDef
brokenReality =
  (treachery ":dark-matter:228" "Broken Reality" Set.FragmentOfCarcosa 3) {cdCardTraits = setFromList [Hazard]}

caveCollapse :: CardDef
caveCollapse =
  (treachery ":dark-matter:229" "Cave Collapse" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hazard]}

chosenByHim :: CardDef
chosenByHim =
  (treachery ":dark-matter:231" "Chosen by Him" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

echoesOfTassildaMatter :: CardDef
echoesOfTassildaMatter =
  (treachery ":dark-matter:232" "Echoes of Tassilda (Matter)" Set.FragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 1
      }

echoesOfTassildaMind :: CardDef
echoesOfTassildaMind =
  (treachery ":dark-matter:233" "Echoes of Tassilda (Mind)" Set.FragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 1
      }

hastursDomain :: CardDef
hastursDomain =
  (treachery ":dark-matter:234" "Hastur's Domain" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Terror]}

irresistibleTruths :: CardDef
irresistibleTruths =
  (treachery ":dark-matter:235" "Irresistible Truths" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

madnessOfCarcosa :: CardDef
madnessOfCarcosa =
  (treachery ":dark-matter:236" "Madness of Carcosa" Set.FragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 2
      }

sceneShifting :: CardDef
sceneShifting =
  (treachery ":dark-matter:237" "Scene Shifting" Set.FragmentOfCarcosa 3) {cdCardTraits = setFromList [Omen]}

songOfYourSoul :: CardDef
songOfYourSoul =
  (treachery ":dark-matter:238" "Song of Your Soul" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

unstableDimension :: CardDef
unstableDimension =
  (treachery ":dark-matter:241" "Unstable Dimension" Set.FragmentOfCarcosa 2) {cdCardTraits = setFromList [Hazard]}

-- starfall
alienation :: CardDef
alienation =
  (treachery ":dark-matter:282" "Alienation" Set.Starfall 2) {cdCardTraits = setFromList [Terror]}

solarEclipse :: CardDef
solarEclipse =
  (treachery ":dark-matter:284" "Solar Eclipse" Set.Starfall 2) {cdCardTraits = setFromList [Power]}

theDarkForest :: CardDef
theDarkForest =
  (treachery ":dark-matter:285" "The Dark Forest" Set.Starfall 2) {cdCardTraits = setFromList [Omen, Endtimes]}

theStarsWereRight :: CardDef
theStarsWereRight =
  (treachery ":dark-matter:287" "The Stars Were Right" Set.Starfall 2) {cdCardTraits = setFromList [Endtimes, Power]}

-- hastur_s_gaze
solarFlare :: CardDef
solarFlare =
  (treachery ":dark-matter:289" "Solar Flare" Set.HastursGaze 3) {cdCardTraits = setFromList [Hazard]}

radiantCrown :: CardDef
radiantCrown =
  (treachery ":dark-matter:290" "Radiant Crown" Set.HastursGaze 2) {cdCardTraits = setFromList [Power]}
