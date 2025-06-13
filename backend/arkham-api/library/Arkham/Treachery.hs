{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Id
import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries

createTreachery :: IsCard a => a -> InvestigatorId -> TreacheryId -> Treachery
createTreachery a iid tid =
  let this = lookupTreachery (toCardCode a) iid tid (toCardId a)
   in overAttrs (\attrs -> attrs {treacheryTaboo = tabooList, treacheryMutated = mutated}) this
 where
  tabooList = case toCard a of
    PlayerCard pc -> pcTabooList pc
    _ -> Nothing
  mutated = case toCard a of
    PlayerCard pc -> tabooMutated tabooList pc
    _ -> Nothing

instance RunMessage Treachery where
  runMessage msg t@(Treachery a) = case msg of
    Revelation iid (isSource t -> True) -> Treachery <$> runMessage msg (overAttrs ((resolvedL %~ insertSet iid) . (waitingL .~ True)) a)
    _ -> Treachery <$> runMessage msg a

lookupTreachery :: CardCode -> InvestigatorId -> TreacheryId -> CardId -> Treachery
lookupTreachery cardCode = case lookup cardCode allTreacheries of
  Nothing -> error $ "Unknown treachery: " <> show cardCode
  Just (SomeTreacheryCard a) -> \i t c -> Treachery $ cbCardBuilder a c (i, t)

instance FromJSON Treachery where
  parseJSON = withObject "Treachery" $ \o -> do
    cCode <- o .: "cardCode"
    withTreacheryCardCode cCode
      $ \(_ :: TreacheryCard a) -> Treachery <$> parseJSON @a (Object o)

withTreacheryCardCode
  :: CardCode -> (forall a. IsTreachery a => TreacheryCard a -> r) -> r
withTreacheryCardCode cCode f = case lookup cCode allTreacheries of
  Nothing -> error $ "Unknown treachery: " <> show cCode
  Just (SomeTreacheryCard a) -> f a

allTreacheries :: Map CardCode SomeTreacheryCard
allTreacheries =
  mapFrom
    someTreacheryCardCode
    [ -- Night of the Zealot
      -- signature
      SomeTreacheryCard coverUp
    , SomeTreacheryCard hospitalDebts
    , SomeTreacheryCard abandonedAndAlone
    , -- weakness
      SomeTreacheryCard amnesia
    , SomeTreacheryCard paranoia
    , SomeTreacheryCard haunted
    , SomeTreacheryCard psychosis
    , SomeTreacheryCard hypochondria
    , -- The Midnight Masks
      SomeTreacheryCard huntingShadow
    , SomeTreacheryCard falseLead
    , -- The Devourer Below
      SomeTreacheryCard umordhothsWrath
    , -- Ghouls
      SomeTreacheryCard graspingHands
    , -- Striking Fear
      SomeTreacheryCard rottingRemains
    , SomeTreacheryCard frozenInFear
    , SomeTreacheryCard dissonantVoices
    , -- Ancient Evils
      SomeTreacheryCard ancientEvils
    , -- Chilling Colds
      SomeTreacheryCard cryptChill
    , SomeTreacheryCard obscuringFog
    , -- Dark Cult
      SomeTreacheryCard mysteriousChanting
    , -- Nightgaunts
      SomeTreacheryCard onWingsOfDarkness
    , -- Locked Doors
      SomeTreacheryCard lockedDoor
    , -- Agents of Hastur
      SomeTreacheryCard theYellowSign
    , -- Agents of Yog Sothoth
      SomeTreacheryCard offerOfPower
    , -- Agents of Cthulhu
      SomeTreacheryCard dreamsOfRlyeh
    , -- The Dunwich Legacy
      -- signature
      SomeTreacheryCard smiteTheWicked
    , SomeTreacheryCard rexsCurse
    , SomeTreacheryCard searchingForIzzie
    , SomeTreacheryCard finalRhapsody
    , SomeTreacheryCard wrackedByNightmares
    , -- weakness
      SomeTreacheryCard indebted
    , SomeTreacheryCard internalInjury
    , SomeTreacheryCard chronophobia
    , -- The House Always Wins
      SomeTreacheryCard somethingInTheDrinks
    , SomeTreacheryCard arousingSuspicions
    , -- Sorcery
      SomeTreacheryCard visionsOfFuturesPast
    , SomeTreacheryCard beyondTheVeil
    , -- Bishop's Thralls
      SomeTreacheryCard lightOfAforgomon
    , -- Dunwich
      SomeTreacheryCard unhallowedCountry
    , SomeTreacheryCard sordidAndSilent
    , -- Whippoorwill
      SomeTreacheryCard eagerForDeath
    , -- Bad Luck
      SomeTreacheryCard cursedLuck
    , SomeTreacheryCard twistOfFate
    , -- Beast Thralls
      SomeTreacheryCard alteredBeast
    , -- Naomi's Crew
      SomeTreacheryCard huntedDown
    , -- The Beyond
      SomeTreacheryCard pushedIntoTheBeyond
    , SomeTreacheryCard terrorFromBeyond
    , SomeTreacheryCard arcaneBarrier
    , -- The Miskatonic Museum
      SomeTreacheryCard shadowSpawned
    , SomeTreacheryCard stalkedInTheDark
    , SomeTreacheryCard passageIntoTheVeil
    , SomeTreacheryCard ephemeralExhibits
    , SomeTreacheryCard slitheringBehindYou
    , -- The Essex County Express
      SomeTreacheryCard acrossSpaceAndTime
    , SomeTreacheryCard clawsOfSteam
    , SomeTreacheryCard brokenRails
    , -- Blood on the Altar
      SomeTreacheryCard kidnapped
    , SomeTreacheryCard psychopompsSong
    , SomeTreacheryCard strangeSigns
    , SomeTreacheryCard rottingRemainsBloodOnTheAltar
    , -- Undimensioned and Unseen
      SomeTreacheryCard toweringBeasts
    , SomeTreacheryCard ruinAndDestruction
    , SomeTreacheryCard attractingAttention
    , SomeTreacheryCard theCreaturesTracks
    , -- Where Doom Awaits
      SomeTreacheryCard ritesHowled
    , SomeTreacheryCard spacesBetween
    , SomeTreacheryCard vortexOfTime
    , -- Lost in Time and Space
      SomeTreacheryCard collapsingReality
    , SomeTreacheryCard wormhole
    , SomeTreacheryCard vastExpanse
    , -- The Path to Carcosa
      -- signature
      SomeTreacheryCard shellShock
    , SomeTreacheryCard starsOfHyades
    , SomeTreacheryCard angeredSpirits
    , SomeTreacheryCard crisisOfIdentity
    , -- weakness
      SomeTreacheryCard overzealous
    , SomeTreacheryCard drawingTheSign
    , -- The Last King
      SomeTreacheryCard fineDining
    , SomeTreacheryCard toughCrowd
    , -- Delusions
      SomeTreacheryCard whispersInYourHeadDismay
    , SomeTreacheryCard whispersInYourHeadDread
    , SomeTreacheryCard whispersInYourHeadAnxiety
    , SomeTreacheryCard whispersInYourHeadDoubt
    , SomeTreacheryCard descentIntoMadness
    , -- Byakhee
      SomeTreacheryCard huntedByByakhee
    , -- Evil Portants
      SomeTreacheryCard blackStarsRise
    , SomeTreacheryCard spiresOfCarcosa
    , SomeTreacheryCard twistedToHisWill
    , -- Hauntings
      SomeTreacheryCard spiritsTorment
    , -- Hastur's Gift
      SomeTreacheryCard danceOfTheYellowKing
    , -- Cult of the Yellow Sign
      SomeTreacheryCard theKingsEdict
    , -- Decay and Filth
      SomeTreacheryCard oozeAndFilth
    , SomeTreacheryCard corrosion
    , -- The Stranger
      SomeTreacheryCard markedByTheSign
    , SomeTreacheryCard thePaleMaskBeckons
    , -- Echoes of the Past
      SomeTreacheryCard ledAstray
    , SomeTreacheryCard theCultsSearch
    , -- The Unspeakable Oath
      SomeTreacheryCard straitjacket
    , SomeTreacheryCard giftOfMadnessPity
    , SomeTreacheryCard giftOfMadnessMisery
    , SomeTreacheryCard wallsClosingIn
    , -- A Phantom of Truth
      SomeTreacheryCard twinSuns
    , SomeTreacheryCard deadlyFate
    , SomeTreacheryCard torturousChords
    , SomeTreacheryCard frozenInFearAPhantomOfTruth
    , SomeTreacheryCard lostSoul
    , -- The Pallid Mask
      SomeTreacheryCard eyesInTheWalls
    , SomeTreacheryCard theShadowBehindYou
    , SomeTreacheryCard thePitBelow
    , -- Black Stars Rise
      SomeTreacheryCard crashingFloods
    , SomeTreacheryCard worldsMerge
    , -- Dim Carcosa
      SomeTreacheryCard dismalCurse
    , SomeTreacheryCard realmOfMadness
    , SomeTreacheryCard theFinalAct
    , SomeTreacheryCard possessionTraitorous
    , SomeTreacheryCard possessionTorturous
    , SomeTreacheryCard possessionMurderous
    , -- Forgotten Age
      -- signature
      SomeTreacheryCard boughtInBlood
    , SomeTreacheryCard callOfTheUnknown
    , SomeTreacheryCard caughtRedHanded
    , SomeTreacheryCard voiceOfTheMessenger
    , -- weaknesses
      SomeTreacheryCard thePriceOfFailure
    , SomeTreacheryCard doomed
    , SomeTreacheryCard accursedFate
    , SomeTreacheryCard theBellTolls
    , -- Rainforest
      SomeTreacheryCard overgrowth
    , SomeTreacheryCard voiceOfTheJungle
    , -- Serpents
      SomeTreacheryCard snakeBite
    , -- Expedition
      SomeTreacheryCard lostInTheWilds
    , SomeTreacheryCard lowOnSupplies
    , -- Agents of Yig
      SomeTreacheryCard curseOfYig
    , -- Guardians of Time
      SomeTreacheryCard arrowsFromTheTrees
    , -- Deadly Trap
      SomeTreacheryCard finalMistake
    , SomeTreacheryCard entombed
    , -- Temporal Flux
      SomeTreacheryCard aTearInTime
    , SomeTreacheryCard lostInTime
    , -- Forgotten Ruins
      SomeTreacheryCard illOmen
    , SomeTreacheryCard ancestralFear
    , SomeTreacheryCard deepDark
    , -- Pnakotic Brotherhood
      SomeTreacheryCard shadowed
    , SomeTreacheryCard wordsOfPower
    , -- Yig's Venom
      SomeTreacheryCard snakescourge
    , SomeTreacheryCard serpentsCall
    , -- Poison
      SomeTreacheryCard creepingPoison
    , SomeTreacheryCard poisoned
    , -- Threads of Fate
      SomeTreacheryCard theSecretMustBeKept
    , SomeTreacheryCard nobodysHome
    , SomeTreacheryCard conspiracyOfBlood
    , -- The Boundary Beyond
      SomeTreacheryCard windowToAnotherTime
    , SomeTreacheryCard timelineDestabilization
    , -- Heart of the Elders
      SomeTreacheryCard pitfall
    , SomeTreacheryCard poisonousSpores
    , --- Pillars of Judgement [hote]
      SomeTreacheryCard ants
    , --- K'n-yan [hote]
      SomeTreacheryCard noTurningBack
    , -- The City of Archives
      SomeTreacheryCard yithianPresence
    , SomeTreacheryCard cruelInterrogations
    , SomeTreacheryCard lostHumanity
    , SomeTreacheryCard captiveMind
    , SomeTreacheryCard outOfBodyExperience
    , -- The Depths of Yoth
      SomeTreacheryCard childrenOfValusia
    , SomeTreacheryCard lightlessShadow
    , SomeTreacheryCard bathophobia
    , SomeTreacheryCard serpentsIre
    , -- Shattered Aeons
      SomeTreacheryCard shatteredAges
    , SomeTreacheryCard betweenWorlds
    , SomeTreacheryCard wrackedByTime
    , SomeTreacheryCard creepingDarkness
    , -- The Circle Undone
      -- signature
      SomeTreacheryCard rationalThought
    , SomeTreacheryCard terribleSecret
    , -- weaknesses
      SomeTreacheryCard the13thVision
    , -- The Watcher
      SomeTreacheryCard watchersGrasp
    , -- Agents of Azathoth
      SomeTreacheryCard daemonicPiping
    , -- Witchcraft
      SomeTreacheryCard diabolicVoices
    , SomeTreacheryCard wracked
    , SomeTreacheryCard bedeviled
    , -- Silver Twilight Lodge
      SomeTreacheryCard mysteriesOfTheLodge
    , -- City of Sins
      SomeTreacheryCard evilPast
    , SomeTreacheryCard centuriesOfSecrets
    , -- Spectral Predators
      SomeTreacheryCard whispersInTheDark
    , -- Trapped Spirits
      SomeTreacheryCard trappedSpirits
    , -- Realm of Death
      SomeTreacheryCard realmOfTorment
    , SomeTreacheryCard shapesInTheMist
    , -- Inexorable Fate
      SomeTreacheryCard terrorInTheNight
    , SomeTreacheryCard fateOfAllFools
    , -- The Secret Name
      SomeTreacheryCard meddlesomeFamiliar
    , SomeTreacheryCard ghostlyPresence
    , SomeTreacheryCard extradimensionalVisions
    , SomeTreacheryCard pulledByTheStars
    , SomeTreacheryCard disquietingDreams
    , -- The Wages of Sin
      SomeTreacheryCard punishment
    , SomeTreacheryCard burdensOfThePast
    , SomeTreacheryCard ominousPortents
    , SomeTreacheryCard graveLight
    , SomeTreacheryCard graveLightSpectral
    , SomeTreacheryCard baneOfTheLiving
    , -- For the Greater Good
      SomeTreacheryCard callToOrder
    , SomeTreacheryCard expulsion
    , SomeTreacheryCard beneathTheLodge
    , SomeTreacheryCard markOfTheOrder
    , -- Union and Disillusion
      SomeTreacheryCard eagerForDeathUnionAndDisillusion
    , SomeTreacheryCard psychopompsSongUnionAndDisillusion
    , SomeTreacheryCard deathApproaches
    , SomeTreacheryCard markedForDeath
    , SomeTreacheryCard watchersGazeUnionAndDisillusion
    , -- In the Clutches of Chaos
      SomeTreacheryCard chaosManifest
    , SomeTreacheryCard primordialGateway
    , SomeTreacheryCard terrorUnleashed
    , --- Secrets of the Universe
      SomeTreacheryCard secretsOfTheBeyond
    , --- Music of the Damned
      SomeTreacheryCard toilAndTrouble
    , -- Before the Black Throne
      SomeTreacheryCard ultimateChaos
    , SomeTreacheryCard whisperedBargain
    , SomeTreacheryCard theEndIsNigh
    , SomeTreacheryCard aWorldInDarkness
    , -- The Dream-Eaters
      -- signature
      SomeTreacheryCard rookieMistake
    , SomeTreacheryCard shockingDiscovery
    , SomeTreacheryCard detachedFromReality
    , -- guardian
      SomeTreacheryCard bloodlust
    , -- weaknesses
      SomeTreacheryCard selfCentered
    , SomeTreacheryCard narcolepsy
    , -- Beyond the Gates of Sleep
      SomeTreacheryCard lostInTheWoods
    , -- Waking Nightmare
      SomeTreacheryCard outbreak
    , -- Agents of Atlach-Nacha
      SomeTreacheryCard willOfTheSpiderMother
    , -- Agents of Nyarlathotep
      SomeTreacheryCard lawOfYgirothChaos
    , SomeTreacheryCard lawOfYgirothDiscord
    , SomeTreacheryCard lawOfYgirothPandemonium
    , -- Whispers of Hypnos
      SomeTreacheryCard whispersOfHypnos
    , -- Dreamer's Curse
      SomeTreacheryCard dreamersCurse
    , SomeTreacheryCard somniphobia
    , SomeTreacheryCard deeperSlumber
    , -- Dreamlands
      SomeTreacheryCard dreamlandsEclipse
    , SomeTreacheryCard prismaticPhenomenon
    , -- Merging Realities
      SomeTreacheryCard nightTerrors
    , SomeTreacheryCard glimpseOfTheUnderworld
    , SomeTreacheryCard threadsOfReality
    , -- Spiders
      SomeTreacheryCard sickeningWebs
    , -- Corsairs
      SomeTreacheryCard huntedByCorsairs
    , -- Zoogs
      SomeTreacheryCard zoogBurrow
    , -- The Search for Kadath
      SomeTreacheryCard songOfTheMagahBird
    , SomeTreacheryCard wondrousLands
    , -- A Thousand Shapes of Horror
      SomeTreacheryCard endlessDescent
    , SomeTreacheryCard indescribableApparition
    , SomeTreacheryCard glowingEyes
    , SomeTreacheryCard deceptiveMemories
    , SomeTreacheryCard secretsInTheAttic
    , -- Dark Side of the Moon
      SomeTreacheryCard closeWatch
    , SomeTreacheryCard forcedIntoHiding
    , SomeTreacheryCard lunarPatrol
    , SomeTreacheryCard falseAwakening
    , -- Point of No Return
      SomeTreacheryCard tasteOfLifeblood
    , SomeTreacheryCard litByDeathFire
    , SomeTreacheryCard unexpectedAmbush
    , SomeTreacheryCard falseAwakeningPointOfNoReturn
    , --- Terror of the Vale
      SomeTreacheryCard dholeTunnel
    , --- Descent into the Pitch
      SomeTreacheryCard shadowOfAtlachNacha
    , -- Where the Gods Dwell
      SomeTreacheryCard whisperingChaosNorth
    , SomeTreacheryCard whisperingChaosEast
    , SomeTreacheryCard whisperingChaosSouth
    , SomeTreacheryCard whisperingChaosWest
    , SomeTreacheryCard myriadForms
    , SomeTreacheryCard restlessJourneyFallacy
    , SomeTreacheryCard restlessJourneyHardship
    , SomeTreacheryCard restlessJourneyLies
    , SomeTreacheryCard abandonedByTheGods
    , -- Weaver of the Cosmos
      SomeTreacheryCard theSpinnerInDarkness
    , SomeTreacheryCard caughtInAWeb
    , SomeTreacheryCard endlessWeaving
    , -- The Innsmouth Conspiracy
      --- signature [tic]
      SomeTreacheryCard crisisOfFaith
    , SomeTreacheryCard sirenCall
    , --- basic weakness [tic]
      SomeTreacheryCard dreadCurse
    , SomeTreacheryCard dayOfReckoning
    , --- The Pit of Despair [tic]
      SomeTreacheryCard blindsense
    , SomeTreacheryCard fromTheDepths
    , --- Agents of Hydrea [tic]
      SomeTreacheryCard psychicPull
    , --- Creatures of the Deep [tic]
      SomeTreacheryCard deepOneAssault
    , --- Rising Tide [tic]
      SomeTreacheryCard undertow
    , SomeTreacheryCard risingTides
    , SomeTreacheryCard riptide
    , --- Fog over Innsmouth [tic]
      SomeTreacheryCard fogOverInnsmouth
    , --- Shattered Memories [tic]
      SomeTreacheryCard macabreMemento
    , SomeTreacheryCard fracturedConsciousness
    , SomeTreacheryCard memoryOfOblivion
    , --- Malfunction [tic]
      SomeTreacheryCard malfunction
    , --- Syzygy [tic]
      SomeTreacheryCard tidalAlignment
    , SomeTreacheryCard syzygy
    , --- The Locals [tic]
      SomeTreacheryCard innsmouthLook
    , SomeTreacheryCard furtiveLocals
    , --- In Too Deep [itd]
      SomeTreacheryCard deepOneInvasion
    , SomeTreacheryCard pulledBack
    , SomeTreacheryCard inundated
    , --- Devil Reef
      SomeTreacheryCard shapesInTheWater
    , SomeTreacheryCard aquaticAmbush
    , SomeTreacheryCard horrorsFromTheDeep
    , SomeTreacheryCard stowaway
    , SomeTreacheryCard draggedUnderDevilReef
    , --- Horror in High Gear
      SomeTreacheryCard bumpyRide
    , SomeTreacheryCard iCantSee
    , SomeTreacheryCard eyesInTheTrees
    , SomeTreacheryCard theyreCatchingUp
    , --- A Light in the Fog [lif]
      SomeTreacheryCard hideousLullaby
    , SomeTreacheryCard kissOfBrine
    , SomeTreacheryCard totality
    , SomeTreacheryCard worthHisSalt
    , SomeTreacheryCard takenCaptive
    , --- The Lair of Dagon [lod]
      SomeTreacheryCard fulfillTheOaths
    , SomeTreacheryCard secretGathering
    , SomeTreacheryCard esotericRitual
    , SomeTreacheryCard heraldsOfTheDeep
    , SomeTreacheryCard stoneBarrier
    , --- Into the Maelstrom [itm]
      SomeTreacheryCard treacherousDepths
    , SomeTreacheryCard conspiracyOfDeepOnes
    , SomeTreacheryCard thalassophobia
    , -- Edge of the Earth
      -- signature
      SomeTreacheryCard theHarbinger
    , SomeTreacheryCard buriedSecrets
    , SomeTreacheryCard burdenOfDestiny
    , SomeTreacheryCard greed
    , -- neutral
      SomeTreacheryCard armInjury
    , SomeTreacheryCard legInjury
    , SomeTreacheryCard panic
    , SomeTreacheryCard stupor
    , --- Ice and Death [eote]
      SomeTreacheryCard apeirophobia
    , SomeTreacheryCard zeroVisibility
    , --- Seeping Nightmares [eote]
      SomeTreacheryCard phantasmagoria
    , --- Fatal Mirage [eote]
      SomeTreacheryCard evanescentMist
    , SomeTreacheryCard anamnesis
    , --- To the Forbidden Peaks [eote]
      SomeTreacheryCard snowfall
    , SomeTreacheryCard avalanche
    , SomeTreacheryCard hangingOnTheEdge
    , SomeTreacheryCard hypothermia
    , --- City of the Elder Things [eote]
      SomeTreacheryCard dawningOfTheTruth
    , SomeTreacheryCard crumblingRuins
    , SomeTreacheryCard frostbitten
    , SomeTreacheryCard possessed
    , --- The Heart of Madness [eote]
      SomeTreacheryCard primevalTerror
    , SomeTreacheryCard rootsOfTheEarth
    , --- Agents of the Unknown [eote]
      SomeTreacheryCard theMadnessWithin
    , --- Creatures in the Ice [eote]
      SomeTreacheryCard kindredMist
    , --- Deadly Weather [eote]
      SomeTreacheryCard antarcticWind
    , SomeTreacheryCard whiteout
    , SomeTreacheryCard polarVortex
    , --- Hazards of Antarctica [eote]
      SomeTreacheryCard iceShaft
    , SomeTreacheryCard throughTheIce
    , --- Elder Things [eote]
      SomeTreacheryCard riseOfTheElderThings
    , --- Left Behind [eote]
      SomeTreacheryCard abandonedToMadness
    , --- Namesless Horrors [eote]
      SomeTreacheryCard blasphemousVisions
    , SomeTreacheryCard glimpseTheUnspeakable
    , SomeTreacheryCard nightmarishVapors
    , --- Miasma [eote]
      SomeTreacheryCard miasmaticTorment
    , SomeTreacheryCard nebulousMiasma
    , --- Penguins [eote]
      SomeTreacheryCard wukWukWuk
    , --- Silence and Mystery [eote]
      SomeTreacheryCard polarMirage
    , SomeTreacheryCard darkAurora
    , --- Tekeli-li [eote]
      SomeTreacheryCard tekelili_223
    , SomeTreacheryCard tekelili_224
    , SomeTreacheryCard tekelili_225
    , SomeTreacheryCard tekelili_226
    , SomeTreacheryCard tekelili_227
    , SomeTreacheryCard tekelili_228
    , SomeTreacheryCard tekelili_229
    , -- The Scarlet Keys
      -- signature
      SomeTreacheryCard selflessToAFault
    , SomeTreacheryCard deafeningSilence
    , SomeTreacheryCard ruinedFilm
    , SomeTreacheryCard burdenOfLeadership
    , -- The Feast of Hemloch Vale
      -- signature [fhv]
      SomeTreacheryCard hastyRepairs
    , SomeTreacheryCard failedExperiment
    , SomeTreacheryCard wheresPa
    , -- The Drowned City
      -- signature [tdc]
      SomeTreacheryCard illDoItMyself
    , SomeTreacheryCard dreamsOfTheFlood
    , SomeTreacheryCard glimpseTheVoid
    , SomeTreacheryCard confiscation
    , SomeTreacheryCard prophecyOfTheEnd
    , SomeTreacheryCard castAdrift
    , -- basic weakness [tdc]
      SomeTreacheryCard downAndOut
    , SomeTreacheryCard morbidCuriosity
    , SomeTreacheryCard disruptivePoltergeist
    , SomeTreacheryCard frenzied
    , -- Return to the Night of the Zealot
      -- Return to the Gathering
      SomeTreacheryCard theZealotsSeal
    , -- Return to the Midnight Masks
      SomeTreacheryCard maskedHorrors
    , -- Return to the Devourer Below
      SomeTreacheryCard vaultOfEarthlyDemise
    , SomeTreacheryCard umordhothsHunger
    , -- Ghouls of Umordhoth
      SomeTreacheryCard chillFromBelow
    , -- The Devourer's Cult
      SomeTreacheryCard maskOfUmordhoth
    , -- Return to the Dunwich Legacy
      SomeTreacheryCard throughTheGates
    , -- Return to the House Always Wins
      SomeTreacheryCard caughtCheating
    , SomeTreacheryCard raiseTheStakes
    , -- Return to the Miskatonic Museum
      SomeTreacheryCard darkBidding
    , SomeTreacheryCard nightBeyondVoid
    , -- Return to Undimensioned and Unseen
      SomeTreacheryCard imperceptableCreature
    , -- Beyond the Threshold
      SomeTreacheryCard hauntingRecollections
    , SomeTreacheryCard aBalefulWelcome
    , SomeTreacheryCard infiniteDoorway
    , -- Resurgent Evils
      SomeTreacheryCard resurgentEvils
    , -- Secret Doors
      SomeTreacheryCard secretDoor
    , -- Creeping Cold
      SomeTreacheryCard inexplicableCold
    , SomeTreacheryCard oppressiveMists
    , -- Erratic Fear
      SomeTreacheryCard violentCommands
    , SomeTreacheryCard idleHands
    , SomeTreacheryCard needForKnowledge
    , -- Return to the Path to Carcosa
      SomeTreacheryCard unspeakableOathBloodthirst
    , SomeTreacheryCard unspeakableOathCuriosity
    , SomeTreacheryCard unspeakableOathCowardice
    , --- Return to the Last King [rtptc]
      SomeTreacheryCard shockingDisplay
    , --- Return to the Unspeakable Oath [rtptc]
      SomeTreacheryCard radicalTreatment
    , SomeTreacheryCard cloudedMemory
    , --- Return to A Phantom of Truth [rtptc]
      SomeTreacheryCard figureInTheShadows
    , --- Return to Black Stars Rise [rtptc]
      SomeTreacheryCard hastursGaze
    , SomeTreacheryCard hastursGrasp
    , --- Delusory Evils [rtptc]
      SomeTreacheryCard delusoryEvils
    , --- Decaying Reality [rtptc]
      SomeTreacheryCard bleedingWalls
    , SomeTreacheryCard fragileThoughts
    , --- Hastur's Envoys [rtptc]
      SomeTreacheryCard theSignOfHastur
    , --- Maddening Delusions [rtptc]
      SomeTreacheryCard visionsInYourMindHorrors
    , SomeTreacheryCard visionsInYourMindFailure
    , SomeTreacheryCard visionsInYourMindDeath
    , SomeTreacheryCard visionsInYourMindHatred
    , SomeTreacheryCard maddeningDelusions
    , --- Neurotic Fear [rtptc]
      SomeTreacheryCard voiceOfTrunembra
    , SomeTreacheryCard melancholy
    , SomeTreacheryCard painfulReflection
    , --- Yog Sothoth's Emissaries
      SomeTreacheryCard eldritchAccord
    , -- Return to the Forgotten Age [rttfa]
      SomeTreacheryCard offerYouCannotRefuse
    , SomeTreacheryCard finePrint
    , SomeTreacheryCard sellYourSoul
    , --- Cult of Pnakotus [rttfa]
      SomeTreacheryCard fromAnotherTime
    , --- Doom Expedition [rttfa]
      SomeTreacheryCard resentfulWilds
    , SomeTreacheryCard bestLaidPlans
    , --- Temporal Hunters [rttfa]
      SomeTreacheryCard mergingTimelines
    , --- Venoumous Hate [rttfa]
      SomeTreacheryCard wrathOfYig
    , -- Return to the Circle Undone
      SomeTreacheryCard damned
    , -- Nathaniel Cho
      SomeTreacheryCard selfDestructive
    , -- Harvey Walters
      SomeTreacheryCard thriceDamnedCuriosity
    , SomeTreacheryCard obsessive
    , -- Jacqueline Fine
      SomeTreacheryCard darkFuture
    , SomeTreacheryCard nihilism
    , -- Stella Clark
      SomeTreacheryCard calledByTheMists
    , SomeTreacheryCard atychiphobia
    , -- Curse of the Rougarou
      SomeTreacheryCard cursedSwamp
    , SomeTreacheryCard spectralMist
    , SomeTreacheryCard draggedUnder
    , SomeTreacheryCard ripplesOnTheSurface
    , SomeTreacheryCard curseOfTheRougarou
    , SomeTreacheryCard onTheProwl
    , SomeTreacheryCard beastOfTheBayou
    , SomeTreacheryCard insatiableBloodlust
    , -- Carnevale of Horror
      SomeTreacheryCard massHysteria
    , SomeTreacheryCard lostInVenice
    , SomeTreacheryCard watchersGaze
    , SomeTreacheryCard chaosInTheWater
    , SomeTreacheryCard mesmerize
    , SomeTreacheryCard abduction
    , SomeTreacheryCard acridMiasma
    , -- Murder at the Excelsior Hotel
      SomeTreacheryCard whatHaveYouDone
    , SomeTreacheryCard noxiousFumes
    , SomeTreacheryCard drivenToMadness
    , SomeTreacheryCard bloodOnYourHands
    , SomeTreacheryCard incriminatingEvidence
    , SomeTreacheryCard violentOutburst
    , --- Alien Interference
      SomeTreacheryCard encephalonSignal
    , --- Vile Experiments
      SomeTreacheryCard harvestedBrain
    , SomeTreacheryCard morbidAwareness
    , --- Sins of the Past
      SomeTreacheryCard chillingPresence
    , -- Side Stories
      --- The Blob That Ate Everything ELSE!
      SomeTreacheryCard realityAcid5U21
    , -- Parallel
      --- All or Nothing
      SomeTreacheryCard hospitalDebtsAdvanced
    , --- By the Book
      SomeTreacheryCard coverUpAdvanced
    , --- Red Tide Rising
      SomeTreacheryCard abandonedAndAloneAdvanced
    , --- On the Road Again
      SomeTreacheryCard hardTimes
    , --- Laid to Rest
      SomeTreacheryCard finalRhapsodyAdvanced
    , --- Path of the Righteous
      SomeTreacheryCard smiteTheWickedAdvanced
    , --- Relics of the Past
      SomeTreacheryCard buriedSecretsAdvanced
    , --- Hunting for Answers
      SomeTreacheryCard rexsCurseAdvanced
    , --- Pistols and Pearls
      SomeTreacheryCard searchingForIzzieAdvanced
    , -- Promo
      --- The Dirge of Reason
      SomeTreacheryCard theDirgeOfReason
    , --- To Fight the Black Wind
      SomeTreacheryCard toFightTheBlackWind
    , --- Blood of Baalshandor
      SomeTreacheryCard yaztaroth
    , --- Dark Revelations
      SomeTreacheryCard liberOmniumFinium
    ]
