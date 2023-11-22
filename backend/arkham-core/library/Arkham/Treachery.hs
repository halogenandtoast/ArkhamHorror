{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Treachery where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Treachery.Runner
import Arkham.Treachery.Treacheries

createTreachery :: IsCard a => a -> InvestigatorId -> TreacheryId -> Treachery
createTreachery a iid tid =
  lookupTreachery (toCardCode a) iid tid (toCardId a)

instance RunMessage Treachery where
  runMessage msg t@(Treachery a) = case msg of
    Revelation iid (isSource t -> True) -> Treachery <$> runMessage msg (overAttrs (resolvedL %~ insertSet iid) a)
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
    , -- Agents of Nyarlathotep
      SomeTreacheryCard lawOfYgirothChaos
    , SomeTreacheryCard lawOfYgirothDiscord
    , SomeTreacheryCard lawOfYgirothPandemonium
    , -- Edge of the Earth
      -- signature
      SomeTreacheryCard theHarbinger
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
    , -- Return to the Path to Carcosa
      SomeTreacheryCard unspeakableOathBloodthirst
    , SomeTreacheryCard unspeakableOathCuriosity
    , SomeTreacheryCard unspeakableOathCowardice
    , -- Return to the Forgotten Age
      SomeTreacheryCard offerYouCannotRefuse
    , SomeTreacheryCard finePrint
    , SomeTreacheryCard sellYourSoul
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
    , -- Promo
      --- Blood of Baalshandor
      SomeTreacheryCard yaztaroth
    ]
