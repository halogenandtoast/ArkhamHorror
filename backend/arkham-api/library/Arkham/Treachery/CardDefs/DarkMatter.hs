module Arkham.Treachery.CardDefs.DarkMatter where

import Arkham.Treachery.CardDefs.Import

-- anachronism
anachronismDarkMatter :: CardDef
anachronismDarkMatter =
  (treachery "z-dark-matter-001" "Anachronism" DarkMatterAnachronism 2) {cdCardTraits = setFromList [Madness, Paradox]}

-- dark_past
hauntingPastDarkMatter :: CardDef
hauntingPastDarkMatter =
  (treachery "z-dark-matter-002" "Haunting Past" DarkMatterDarkPast 2) {cdCardTraits = setFromList [Scheme]}

reminiscencePledgeDarkMatter :: CardDef
reminiscencePledgeDarkMatter =
  (treachery "z-dark-matter-003" "Reminiscence (Pledge)" DarkMatterDarkPast 1) {cdCardTraits = setFromList [Pact]}

reminiscenceSecretsDarkMatter :: CardDef
reminiscenceSecretsDarkMatter =
  (treachery "z-dark-matter-004" "Reminiscence (Secrets)" DarkMatterDarkPast 1) {cdCardTraits = setFromList [Pact]}

reminiscenceCovenantDarkMatter :: CardDef
reminiscenceCovenantDarkMatter =
  (treachery "z-dark-matter-005" "Reminiscence (Covenant)" DarkMatterDarkPast 1) {cdCardTraits = setFromList [Pact]}

-- deep_space
callOfTheVoidDarkMatter :: CardDef
callOfTheVoidDarkMatter =
  (treachery "z-dark-matter-007" "Call of the Void" DarkMatterDeepSpace 1) {cdCardTraits = setFromList [Omen]}

coldVacuumDarkMatter :: CardDef
coldVacuumDarkMatter =
  (treachery "z-dark-matter-008" "Cold Vacuum" DarkMatterDeepSpace 2) {cdCardTraits = setFromList [Hazard]}

micrometeoroidDarkMatter :: CardDef
micrometeoroidDarkMatter =
  (treachery "z-dark-matter-009" "Micrometeoroid" DarkMatterDeepSpace 2) {cdCardTraits = setFromList [Hazard]}

theColorsOfSpaceDarkMatter :: CardDef
theColorsOfSpaceDarkMatter =
  (treachery "z-dark-matter-010" "The Colors of Space" DarkMatterDeepSpace 2) {cdCardTraits = setFromList [Hazard]}

-- endtimes
grimFutureDarkMatter :: CardDef
grimFutureDarkMatter =
  (treachery "z-dark-matter-011" "Grim Future" DarkMatterEndtimes 2) {cdCardTraits = setFromList [Endtimes]}

futureEvilsDarkMatter :: CardDef
futureEvilsDarkMatter =
  (treachery "z-dark-matter-012" "Future Evils" DarkMatterEndtimes 3) {cdCardTraits = setFromList [Omen, Endtimes]}

-- the_tatterdemalion
artificialGravityMalfunctionDarkMatter :: CardDef
artificialGravityMalfunctionDarkMatter =
  (treachery "z-dark-matter-043" "Artificial Gravity Malfunction" DarkMatterTheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

cabinPressureDarkMatter :: CardDef
cabinPressureDarkMatter =
  (treachery "z-dark-matter-044" "Cabin Pressure" DarkMatterTheTatterdemalion 2) {cdCardTraits = setFromList [Madness]}

coolantLeakDarkMatter :: CardDef
coolantLeakDarkMatter =
  (treachery "z-dark-matter-045" "Coolant Leak" DarkMatterTheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

decompressionDarkMatter :: CardDef
decompressionDarkMatter =
  (treachery "z-dark-matter-046" "Decompression" DarkMatterTheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

highRadiationLevelsDarkMatter :: CardDef
highRadiationLevelsDarkMatter =
  (treachery "z-dark-matter-047" "High Radiation Levels" DarkMatterTheTatterdemalion 2) {cdCardTraits = setFromList [Hazard]}

-- artificial_intelligence
allSeeingEyeDarkMatter :: CardDef
allSeeingEyeDarkMatter =
  (treachery "z-dark-matter-048" "All-Seeing Eye" DarkMatterArtificialIntelligence 3) {cdCardTraits = setFromList [AI]}

electricSurgeDarkMatter :: CardDef
electricSurgeDarkMatter =
  (treachery "z-dark-matter-049" "Electric Surge" DarkMatterArtificialIntelligence 2) {cdCardTraits = setFromList [Hazard]}

hallucinatoryHologramsDarkMatter :: CardDef
hallucinatoryHologramsDarkMatter =
  (treachery "z-dark-matter-050" "Hallucinatory Holograms" DarkMatterArtificialIntelligence 2) {cdCardTraits = setFromList [Terror]}

predictiveAlgorithmDarkMatter :: CardDef
predictiveAlgorithmDarkMatter =
  (treachery "z-dark-matter-051" "Predictive Algorithm" DarkMatterArtificialIntelligence 2) {cdCardTraits = setFromList [AI, Scheme]}

-- electric_nightmare
desyncDarkMatter :: CardDef
desyncDarkMatter =
  (weakness "z-dark-matter-066" "Desync")
      { cdCardTraits = setFromList [Madness]
      , cdEncounterSet = Just DarkMatterElectricNightmare
      , cdEncounterSetQuantity = Just 4
      }

digitalCorrosionDarkMatter :: CardDef
digitalCorrosionDarkMatter =
  (treachery "z-dark-matter-078" "Digital Corrosion" DarkMatterElectricNightmare 2) {cdCardTraits = setFromList [Virtual, Hazard]}

decoherenceDarkMatter :: CardDef
decoherenceDarkMatter =
  (treachery "z-dark-matter-080" "Decoherence" DarkMatterElectricNightmare 2) {cdCardTraits = setFromList [Virtual, Terror]}

duplicationDarkMatter :: CardDef
duplicationDarkMatter =
  (treachery "z-dark-matter-081" "Duplication" DarkMatterElectricNightmare 2) {cdCardTraits = setFromList [Scheme]}

nonEuclideanGeometryDarkMatter :: CardDef
nonEuclideanGeometryDarkMatter =
  (treachery "z-dark-matter-084" "Non-Euclidean Geometry" DarkMatterElectricNightmare 2) {cdCardTraits = setFromList [Hazard]}

-- the_boogeyman
comeCLOSERDarkMatter :: CardDef
comeCLOSERDarkMatter =
  (treachery "z-dark-matter-087" "COME CLOSER" DarkMatterTheBoogeyman 2) {cdCardTraits = setFromList [Nightmare, Terror]}

rememberMEDarkMatter :: CardDef
rememberMEDarkMatter =
  (treachery "z-dark-matter-088" "REMEMBER ME?" DarkMatterTheBoogeyman 2) {cdCardTraits = setFromList [Nightmare, Terror]}

surpriseDarkMatter :: CardDef
surpriseDarkMatter =
  (treachery "z-dark-matter-089" "SURPRISE!" DarkMatterTheBoogeyman 2) {cdCardTraits = setFromList [Virtual, Nightmare]}

-- lost_quantum
entangledDarkMatter :: CardDef
entangledDarkMatter =
  (treachery "z-dark-matter-107" "Entangled" DarkMatterLostQuantum 3) {cdCardTraits = setFromList [Quantum]}

incomprehensibleDarkMatter :: CardDef
incomprehensibleDarkMatter =
  (treachery "z-dark-matter-109" "Incomprehensible" DarkMatterLostQuantum 2) {cdCardTraits = setFromList [Paradox]}

paradoxicalThreatDarkMatter :: CardDef
paradoxicalThreatDarkMatter =
  (treachery "z-dark-matter-111" "Paradoxical Threat" DarkMatterLostQuantum 2) {cdCardTraits = setFromList [Omen]}

quantumCollapseDarkMatter :: CardDef
quantumCollapseDarkMatter =
  (treachery "z-dark-matter-112" "Quantum Collapse" DarkMatterLostQuantum 3) {cdCardTraits = setFromList [Quantum]}

radioactiveDecayDarkMatter :: CardDef
radioactiveDecayDarkMatter =
  (treachery "z-dark-matter-114" "Radioactive Decay" DarkMatterLostQuantum 2) {cdCardTraits = setFromList [Hazard]}

-- in_the_shadow_of_earth
anothersWoeDarkMatter :: CardDef
anothersWoeDarkMatter =
  (treachery "z-dark-matter-145" "Another's Woe" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Omen]}

contaminationDarkMatter :: CardDef
contaminationDarkMatter =
  (treachery "z-dark-matter-146" "Contamination" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Injury, Madness]}

fromTheDarkDarkMatter :: CardDef
fromTheDarkDarkMatter =
  (treachery "z-dark-matter-148" "From the Dark" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Tactic]}

hopelessDarkMatter :: CardDef
hopelessDarkMatter =
  (treachery "z-dark-matter-149" "Hopeless" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Terror]}

infectionDarkMatter :: CardDef
infectionDarkMatter =
  (treachery "z-dark-matter-150" "Infection" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Injury, Curse]}

paleBlueDotDarkMatter :: CardDef
paleBlueDotDarkMatter =
  (treachery "z-dark-matter-152" "Pale Blue Dot" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Terror]}

perfectImitationDarkMatter :: CardDef
perfectImitationDarkMatter =
  (treachery "z-dark-matter-153" "Perfect Imitation" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Trap]}

scrambledDarkMatter :: CardDef
scrambledDarkMatter =
  (treachery "z-dark-matter-155" "Scrambled" DarkMatterInTheShadowOfEarth 2) {cdCardTraits = setFromList [Blunder]}

-- strange_moons
alienAidDarkMatter :: CardDef
alienAidDarkMatter =
  (treachery "z-dark-matter-182" "Alien Aid" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [MiGo, Pact]}

closeEncountersDarkMatter :: CardDef
closeEncountersDarkMatter =
  (treachery "z-dark-matter-183" "Close Encounters" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [Hazard]}

innocentMishapDarkMatter :: CardDef
innocentMishapDarkMatter =
  (treachery "z-dark-matter-184" "Innocent Mishap" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [Blunder]}

lostInTranslationDarkMatter :: CardDef
lostInTranslationDarkMatter =
  (treachery "z-dark-matter-185" "Lost in Translation" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [Blunder]}

miGoExperimentsDarkMatter :: CardDef
miGoExperimentsDarkMatter =
  (treachery "z-dark-matter-186" "Mi-Go Experiments" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [MiGo, Terror]}

simulationDiscrepancyDarkMatter :: CardDef
simulationDiscrepancyDarkMatter =
  (treachery "z-dark-matter-188" "Simulation Discrepancy" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [Blunder, Obstacle]}

toxicPitsDarkMatter :: CardDef
toxicPitsDarkMatter =
  (treachery "z-dark-matter-189" "Toxic Pits" DarkMatterStrangeMoons 2) {cdCardTraits = setFromList [Hazard]}

-- interstellar_predators
extraterrestrialAssaultDarkMatter :: CardDef
extraterrestrialAssaultDarkMatter =
  (treachery "z-dark-matter-190" "Extraterrestrial Assault" DarkMatterInterstellarPredators 2) {cdCardTraits = setFromList [Pact]}

-- the_machine_in_yellow
darkReflectionsMalingererDarkMatter :: CardDef
darkReflectionsMalingererDarkMatter =
  (treachery "z-dark-matter-202" "Dark Reflections (Malingerer)" DarkMatterTheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsMurdererDarkMatter :: CardDef
darkReflectionsMurdererDarkMatter =
  (treachery "z-dark-matter-203" "Dark Reflections (Murderer)" DarkMatterTheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsSycophantDarkMatter :: CardDef
darkReflectionsSycophantDarkMatter =
  (treachery "z-dark-matter-204" "Dark Reflections (Sycophant)" DarkMatterTheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

darkReflectionsZealotDarkMatter :: CardDef
darkReflectionsZealotDarkMatter =
  (treachery "z-dark-matter-205" "Dark Reflections (Zealot)" DarkMatterTheMachineInYellow 1) {cdCardTraits = setFromList [Madness]}

delusionalMadnessDarkMatter :: CardDef
delusionalMadnessDarkMatter =
  (treachery "z-dark-matter-206" "Delusional Madness" DarkMatterTheMachineInYellow 3) {cdCardTraits = setFromList [Madness]}

fathomlessRegretsDarkMatter :: CardDef
fathomlessRegretsDarkMatter =
  (treachery "z-dark-matter-207" "Fathomless Regrets" DarkMatterTheMachineInYellow 2) {cdCardTraits = setFromList [Terror]}

forbiddingPromisesDarkMatter :: CardDef
forbiddingPromisesDarkMatter =
  (treachery "z-dark-matter-208" "Forbidding Promises" DarkMatterTheMachineInYellow 2) {cdCardTraits = setFromList [Terror]}

persistenceOfMemoryDarkMatter :: CardDef
persistenceOfMemoryDarkMatter =
  (treachery "z-dark-matter-209" "Persistence of Memory" DarkMatterTheMachineInYellow 2)

perspectiveSwitchDarkMatter :: CardDef
perspectiveSwitchDarkMatter =
  (treachery "z-dark-matter-210" "Perspective Switch" DarkMatterTheMachineInYellow 2) {cdCardTraits = setFromList [Madness]}

-- fragment_of_carcosa
brokenRealityDarkMatter :: CardDef
brokenRealityDarkMatter =
  (treachery "z-dark-matter-231" "Broken Reality" DarkMatterFragmentOfCarcosa 3) {cdCardTraits = setFromList [Hazard]}

caveCollapseDarkMatter :: CardDef
caveCollapseDarkMatter =
  (treachery "z-dark-matter-232" "Cave Collapse" DarkMatterFragmentOfCarcosa 2) {cdCardTraits = setFromList [Hazard]}

chosenByHimDarkMatter :: CardDef
chosenByHimDarkMatter =
  (treachery "z-dark-matter-234" "Chosen by Him" DarkMatterFragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

echoesOfTassildaMatterDarkMatter :: CardDef
echoesOfTassildaMatterDarkMatter =
  (treachery "z-dark-matter-235" "Echoes of Tassilda (Matter)" DarkMatterFragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 1
      }

echoesOfTassildaMindDarkMatter :: CardDef
echoesOfTassildaMindDarkMatter =
  (treachery "z-dark-matter-236" "Echoes of Tassilda (Mind)" DarkMatterFragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 1
      }

hastursDomainDarkMatter :: CardDef
hastursDomainDarkMatter =
  (treachery "z-dark-matter-237" "Hastur's Domain" DarkMatterFragmentOfCarcosa 2) {cdCardTraits = setFromList [Terror]}

irresistibleTruthsDarkMatter :: CardDef
irresistibleTruthsDarkMatter =
  (treachery "z-dark-matter-238" "Irresistible Truths" DarkMatterFragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

madnessOfCarcosaDarkMatter :: CardDef
madnessOfCarcosaDarkMatter =
  (treachery "z-dark-matter-239" "Madness of Carcosa" DarkMatterFragmentOfCarcosa 1)
      { cdCardTraits = setFromList [Madness]
      , cdVictoryPoints = Just 2
      }

sceneShiftingDarkMatter :: CardDef
sceneShiftingDarkMatter =
  (treachery "z-dark-matter-240" "Scene Shifting" DarkMatterFragmentOfCarcosa 3) {cdCardTraits = setFromList [Omen]}

songOfYourSoulDarkMatter :: CardDef
songOfYourSoulDarkMatter =
  (treachery "z-dark-matter-241" "Song of Your Soul" DarkMatterFragmentOfCarcosa 2) {cdCardTraits = setFromList [Hex]}

unstableDimensionDarkMatter :: CardDef
unstableDimensionDarkMatter =
  (treachery "z-dark-matter-244" "Unstable Dimension" DarkMatterFragmentOfCarcosa 2) {cdCardTraits = setFromList [Hazard]}

-- starfall
alienationDarkMatter :: CardDef
alienationDarkMatter =
  (treachery "z-dark-matter-285" "Alienation" DarkMatterStarfall 2) {cdCardTraits = setFromList [Terror]}

solarEclipseDarkMatter :: CardDef
solarEclipseDarkMatter =
  (treachery "z-dark-matter-287" "Solar Eclipse" DarkMatterStarfall 2) {cdCardTraits = setFromList [Power]}

theDarkForestDarkMatter :: CardDef
theDarkForestDarkMatter =
  (treachery "z-dark-matter-288" "The Dark Forest" DarkMatterStarfall 2) {cdCardTraits = setFromList [Omen, Endtimes]}

theStarsWereRightDarkMatter :: CardDef
theStarsWereRightDarkMatter =
  (treachery "z-dark-matter-290" "The Stars Were Right" DarkMatterStarfall 2) {cdCardTraits = setFromList [Endtimes, Power]}

-- hastur_s_gaze
solarFlareDarkMatter :: CardDef
solarFlareDarkMatter =
  (treachery "z-dark-matter-292" "Solar Flare" DarkMatterHastursGaze 3) {cdCardTraits = setFromList [Hazard]}

radiantCrownDarkMatter :: CardDef
radiantCrownDarkMatter =
  (treachery "z-dark-matter-293" "Radiant Crown" DarkMatterHastursGaze 2) {cdCardTraits = setFromList [Power]}
