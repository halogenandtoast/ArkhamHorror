{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Event where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Events
import Arkham.Event.Runner
import Arkham.Id

createEvent :: IsCard a => a -> InvestigatorId -> EventId -> Event
createEvent a iid eid =
  let this = lookupEvent (toCardCode a) iid eid (toCardId a)
   in overAttrs (\attrs -> attrs {eventCustomizations = customizations}) this
 where
  customizations = case toCard a of
    PlayerCard pc -> pcCustomizations pc
    _ -> mempty

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

lookupEvent :: CardCode -> InvestigatorId -> EventId -> CardId -> Event
lookupEvent cardCode = case lookup cardCode allEvents of
  Nothing -> error $ "Unknown event: " <> show cardCode
  Just (SomeEventCard a) -> \i e c -> Event $ cbCardBuilder a c (i, e)

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> do
    cCode <- o .: "cardCode"
    withEventCardCode cCode
      $ \(_ :: EventCard a) -> Event <$> parseJSON @a (Object o)

withEventCardCode
  :: CardCode -> (forall a. IsEvent a => EventCard a -> r) -> r
withEventCardCode cCode f = case lookup cCode allEvents of
  Nothing -> error $ "Unknown event: " <> show cCode
  Just (SomeEventCard a) -> f a

allEvents :: Map CardCode SomeEventCard
allEvents =
  mapFrom
    someEventCardCode
    [ -- Night of the Zealot
      --- signature [notz]
      SomeEventCard onTheLam
    , SomeEventCard darkMemory
    , --- guardian [notz]
      SomeEventCard evidence
    , SomeEventCard dodge
    , SomeEventCard dynamiteBlast
    , SomeEventCard extraAmmunition1
    , --- seeker [notz]
      SomeEventCard mindOverMatter
    , SomeEventCard workingAHunch
    , SomeEventCard barricade
    , SomeEventCard crypticResearch4
    , --- rogue [notz]
      SomeEventCard elusive
    , SomeEventCard backstab
    , SomeEventCard sneakAttack
    , SomeEventCard sureGamble3
    , SomeEventCard hotStreak4
    , --- mystic [notz]
      SomeEventCard drawnToTheFlame
    , SomeEventCard wardOfProtection
    , SomeEventCard blindingLight
    , SomeEventCard mindWipe1
    , SomeEventCard blindingLight2
    , --- survivor [notz]
      SomeEventCard cunningDistraction
    , SomeEventCard lookWhatIFound
    , SomeEventCard lucky
    , SomeEventCard closeCall2
    , SomeEventCard lucky2
    , SomeEventCard willToSurvive3
    , --- neutral [notz]
      SomeEventCard emergencyCache
    , -- The Dunwich Legacy
      --- signature [tdl]
      SomeEventCard searchForTheTruth
    , --- guardian [tdl]
      SomeEventCard taunt
    , SomeEventCard teamwork
    , SomeEventCard taunt2
    , --- seeker [tdl]
      SomeEventCard shortcut
    , SomeEventCard seekingAnswers
    , --- rogue [tdl]
      SomeEventCard thinkOnYourFeet
    , --- mystic [tdl]
      SomeEventCard bindMonster2
    , --- survivor [tdl]
      SomeEventCard baitAndSwitch
    , -- The Miskatonic Museum
      --- guardian [tmm]
      SomeEventCard emergencyAid
    , --- seeker [tmm]
      SomeEventCard iveGotAPlan
    , --- rogue [tmm]
      SomeEventCard contraband
    , --- mystic [tmm]
      SomeEventCard delveTooDeep
    , --- survivor [tmm]
      SomeEventCard oops
    , SomeEventCard flare1
    , -- The Essex County Express
      --- guardian [tece]
      SomeEventCard standTogether3
    , --- rogue [tece]
      SomeEventCard imOuttaHere
    , --- mystic [tece]
      SomeEventCard hypnoticGaze
    , --- survivor [tece]
      SomeEventCard lure1
    , -- Blood on the Altar
      --- guardian [bota]
      SomeEventCard preparedForTheWorst
    , --- seeker [bota]
      SomeEventCard preposterousSketches
    , --- neutral [bota]
      SomeEventCard emergencyCache2
    , -- Undimensioned and Unseen
      --- guardian [uau]
      SomeEventCard ifItBleeds
    , --- seeker [uau]
      SomeEventCard exposeWeakness1
    , -- Where Doom Awaits
      --- guardian [wda]
      SomeEventCard iveHadWorse4
    , --- rogue [wda]
      SomeEventCard aceInTheHole3
    , --- mystic [wda]
      SomeEventCard moonlightRitual
    , --- survivor [wda]
      SomeEventCard aChanceEncounter
    , --- neutral [wda]
      SomeEventCard momentOfRespite3
    , -- Lost in Time and Space
      --- guardian [litas]
      SomeEventCard monsterSlayer5
    , --- seeker [litas]
      SomeEventCard decipheredReality5
    , --- mystic [litas]
      SomeEventCard wardOfProtection5
    , -- The Path to Carcosa
      --- signature [ptc]
      SomeEventCard thePaintedWorld
    , SomeEventCard buryThemDeep
    , SomeEventCard improvisation
    , --- guardian [ptc]
      SomeEventCard letMeHandleThis
    , SomeEventCard everVigilant1
    , --- seeker [ptc]
      SomeEventCard noStoneUnturned
    , --- rogue [ptc]
      SomeEventCard sleightOfHand
    , SomeEventCard daringManeuver
    , --- mystic [ptc]
      SomeEventCard uncageTheSoul
    , SomeEventCard astralTravel
    , --- survivor [ptc]
      SomeEventCard hidingSpot
    , -- Echoes of the Past
      --- guardian [eotp]
      SomeEventCard heroicRescue
    , --- seeker [eotp]
      SomeEventCard anatomicalDiagrams
    , -- The Unspeakable Oath
      --- guardian [tuo]
      SomeEventCard ambush1
    , --- seeker [tuo]
      SomeEventCard forewarned1
    , --- rogue [tuo]
      SomeEventCard sneakAttack2
    , --- mystic [tuo]
      SomeEventCard stormOfSpirits
    , --- survivor [tuo]
      SomeEventCard fightOrFlight
    , SomeEventCard aTestOfWill1
    , SomeEventCard devilsLuck
    , --- neutral [tuo]
      SomeEventCard callingInFavors
    , -- A Phantom of Truth
      --- guardian [apot]
      SomeEventCard illSeeYouInHell
    , --- seeker [apot]
      SomeEventCard logicalReasoning
    , --- rogue [apot]
      SomeEventCard cheapShot
    , --- mystic [apot]
      SomeEventCard quantumFlux
    , SomeEventCard recharge2
    , --- survivor [apot]
      SomeEventCard snareTrap2
    , -- The Pallid Mask
      --- guardian [tpm]
      SomeEventCard manoAMano1
    , --- seeker [tpm]
      SomeEventCard shortcut2
    , --- survivor [tpm]
      SomeEventCard waylay
    , SomeEventCard aChanceEncounter2
    , --- neutral [tpm]
      SomeEventCard emergencyCache3
    , -- Black Stars Rise
      --- guardian [bsr]
      SomeEventCard onTheHunt
    , --- seeker [bsr]
      SomeEventCard guidance
    , --- rogue [bsr]
      SomeEventCard narrowEscape
    , --- mystic [bsr]
      SomeEventCard wardOfProtection2
    , --- survivor [bsr]
      SomeEventCard trueSurvivor3
    , -- Dim Carcosa
      --- guardian [dca]
      SomeEventCard eatLead2
    , --- seeker [dca]
      SomeEventCard eideticMemory3
    , SomeEventCard noStoneUnturned5
    , --- rogue [dca]
      SomeEventCard cheatDeath5
    , --- mystic [dca]
      SomeEventCard timeWarp2
    , --- survivor [dca]
      SomeEventCard infighting3
    , -- The Forgotten Age
      --- signature [tfa]
      SomeEventCard smuggledGoods
    , --- guardian [tfa]
      SomeEventCard trusted
    , SomeEventCard reliable1
    , --- seeker [tfa]
      SomeEventCard unearthTheAncients
    , --- rogue [tfa]
      SomeEventCard eavesdrop
    , SomeEventCard youHandleThisOne
    , --- mystic [tfa]
      SomeEventCard darkProphecy
    , --- survivor [tfa]
      SomeEventCard improvisedWeapon
    , SomeEventCard dumbLuck
    , --- weakness [tfa]
      SomeEventCard darkPact
    , -- Thread of Fate
      --- guardian [tof]
      SomeEventCard sceneOfTheCrime
    , SomeEventCard marksmanship1
    , --- seeker [tof]
      SomeEventCard persuasion
    , --- mystic [tof]
      SomeEventCard counterspell2
    , --- survivor [tof]
      SomeEventCard perseverance
    , -- The Boundary Beyond
      --- guardian [tbb]
      SomeEventCard secondWind
    , --- seeker [tbb]
      SomeEventCard truthFromFiction
    , -- Heart of the Elders
      --- guardian [hote]
      SomeEventCard customAmmunition3
    , --- seeker [hote]
      SomeEventCard exposeWeakness3
    , --- mystic [hote]
      SomeEventCard premonition
    , --- survivor [hote]
      SomeEventCard liveAndLearn
    , SomeEventCard againstAllOdds2
    , -- The City of Archives
      --- rogue [tcoa]
      SomeEventCard slipAway
    , SomeEventCard payDay1
    , --- mystic [tcoa]
      SomeEventCard sacrifice1
    , -- The Depths of Yoth
      --- guardian [tdoy]
      SomeEventCard bloodEclipse3
    , --- rogue [tdoy]
      SomeEventCard coupDeGrace
    , --- survivor [tdoy]
      SomeEventCard wingingIt
    , --- Shattered Aeons
      --- seeker [sha]
      SomeEventCard vantagePoint
    , --- survivor [sha]
      SomeEventCard impromptuBarrier
    , SomeEventCard alterFate3
    , -- The Circle Undone
      --- signature [tcu]
      SomeEventCard unsolvedCase
    , SomeEventCard lodgeDebts
    , SomeEventCard darkInsight
    , SomeEventCard imDoneRunnin
    , SomeEventCard mystifyingSong
    , --- guardian [tcu]
      SomeEventCard interrogate
    , SomeEventCard delayTheInevitable
    , --- seeker [tcu]
      SomeEventCard connectTheDots
    , --- rogue [tcu]
      SomeEventCard moneyTalks
    , --- mystic [tcu]
      SomeEventCard denyExistence
    , SomeEventCard eldritchInspiration
    , --- survivor [tcu]
      SomeEventCard actOfDesperation
    , -- The Secret Name
      --- seeker [tsn]
      SomeEventCard crackTheCase
    , --- rogue [tsn]
      SomeEventCard intelReport
    , --- mystic [tsn]
      SomeEventCard banish1
    , -- The Wages of Sin
      -- guardian [tws]
      SomeEventCard wellMaintained1
    , -- rogue [tws]
      SomeEventCard swiftReflexes
    , -- survivor [tws]
      SomeEventCard bellyOfTheBeast
    , -- Union and Disillusion
      --- guardian [uad]
      SomeEventCard warningShot
    , SomeEventCard telescopicSight3
    , --- seeker [uad]
      SomeEventCard knowledgeIsPower
    , --- rogue [uad]
      SomeEventCard decoy
    , --- survivor [uad]
      SomeEventCard fortuneOrFate2
    , -- In the Clutches of Chaos
      --- seeker [uad]
      SomeEventCard ghastlyRevelation
    , --- rogue [uad]
      SomeEventCard smallFavor
    , --- mystic [icc]
      SomeEventCard denyExistence5
    , --- survivor [icc]
      SomeEventCard trialByFire
    , SomeEventCard baitAndSwitch3
    , -- Before the Black Throne
      --- guardian [bbt]
      SomeEventCard soothingMelody
    , SomeEventCard iveHadWorse2
    , --- seeker [bbt]
      SomeEventCard bloodRite
    , SomeEventCard glimpseTheUnthinkable5
    , --- rogue [bbt]
      SomeEventCard youOweMeOne
    , --- survivor [bbt]
      SomeEventCard lure2
    , SomeEventCard eucatastrophe3
    , -- The Dream-Eaters
      --- signature [tde]
      SomeEventCard occultEvidence
    , --- seeker [tde]
      SomeEventCard astoundingRevelation
    , --- rogue [tde]
      SomeEventCard easyMark1
    , --- mystic [tde]
      SomeEventCard stargazing1
    , SomeEventCard theStarsAreRight
    , SomeEventCard openGate
    , --- survivor [tde]
      SomeEventCard fortuitousDiscovery
    , -- The Search for Kadath
      --- guardian [sfk]
      SomeEventCard firstWatch
    , --- rogue [sfk]
      SomeEventCard followed
    , --- mystic [sfk]
      SomeEventCard readTheSigns
    , -- A Thousand Shapes of Horror
      --- guardian [tsh]
      SomeEventCard foolMeOnce1
    , --- rogue [tsh]
      SomeEventCard letGodSortThemOut
    , SomeEventCard swiftReload2
    , --- mystic [tsh]
      SomeEventCard etherealForm
    , --- survivor [tsh]
      SomeEventCard scroungeForSupplies
    , -- Dark Side of the Moon
      --- seeker [dsm]
      SomeEventCard practiceMakesPerfect
    , SomeEventCard extensiveResearch1
    , --- mystic [dsm]
      SomeEventCard spectralRazor
    , SomeEventCard wordOfCommand2
    , --- neutral [dsm]
      SomeEventCard lucidDreaming2
    , -- Point of No Return
      --- guardian [pnr]
      SomeEventCard heroicRescue2
    , --- survivor [pnr]
      SomeEventCard aGlimmerOfHope
    , -- Where the Gods Dwell
      --- survivor [wgd]
      SomeEventCard nothingLeftToLose3
    , -- The Innsmouth Conspiracy
      --- signature [tic]
      SomeEventCard obscureStudies
    , SomeEventCard inTheShadows
    , --- guardian [tic]
      SomeEventCard handOfFate
    , --- seeker [tic]
      SomeEventCard deepKnowledge
    , --- rogue [tic]
      SomeEventCard faustianBargain
    , --- mystic [tic]
      SomeEventCard tidesOfFate
    , SomeEventCard wardOfRadiance
    , --- survivor [tic]
      SomeEventCard keepFaith
    , --- neutral [tic]
      SomeEventCard temptFate
    , -- In Too Deep
      --- guardian [itd]
      SomeEventCard righteousHunt1
    , --- seeker [itd]
      SomeEventCard stirringUpTrouble1
    , --- rogue [itd]
      SomeEventCard breakingAndEntering
    , -- Devil Reef
      --- guardian [def]
      SomeEventCard radiantSmite1
    , --- seeker [def]
      SomeEventCard theTruthBeckons
    , SomeEventCard gazeOfOuraxsh2
    , --- rogue [def]
      SomeEventCard underSurveillance1
    , --- survivor [def]
      SomeEventCard butterflyEffect1
    , SomeEventCard thirdTimesACharm2
    , --- neutral [def]
      SomeEventCard manipulateDestiny2
    , -- Horror in High Gear
      --- rogue [hhg]
      SomeEventCard riastrad1
    , --  A Light in the Fog
      --- survivor [hhg]
      SomeEventCard harmonyRestored2
    , --  The Lair of Dagon
      --- guardian [lod]
      SomeEventCard enchantWeapon3
    , --- seeker [lod]
      SomeEventCard theStygianEye3
    , --- survivor [lod]
      SomeEventCard aWatchfulPeace3
    , --  Into the Maelstrom
      --- guardian [itm]
      SomeEventCard hallow3
    , --- mystic [itm]
      SomeEventCard riteOfEquilibrium5
    , --- survivor [itm]
      SomeEventCard shrineOfTheMoirai3
    , -- Edge of the Earth
      --- guardian [eote]
      SomeEventCard toeToToe
    , SomeEventCard getBehindMe
    , SomeEventCard gangUp1
    , SomeEventCard sweepingKick1
    , SomeEventCard dodge2
    , SomeEventCard fangOfTyrthrha4
    , SomeEventCard onTheHunt3
    , --- seeker [eote]
      SomeEventCard writtenInTheStars
    , SomeEventCard joinTheCaravan1
    , SomeEventCard unearthTheAncients2
    , --- rogue [eote]
      SomeEventCard scoutAhead
    , SomeEventCard twentyOneOrBust
    , SomeEventCard counterespionage1
    , SomeEventCard cheatTheSystem1
    , SomeEventCard untimelyTransaction1
    , SomeEventCard moneyTalks2
    , SomeEventCard blackMarket2
    , --- mystic [eote]
      SomeEventCard meditativeTrance
    , SomeEventCard windsOfPower1
    , SomeEventCard foresight1
    , SomeEventCard parallelFates2
    , --- survivor [eote]
      SomeEventCard juryRig
    , SomeEventCard burnAfterReading1
    , SomeEventCard bloodWillHaveBlood2
    , SomeEventCard fendOff3
    , --- guardian/seeker [eote]
      SomeEventCard onTheTrail1
    , SomeEventCard onTheTrail3
    , --- guardian/rogue [eote]
      SomeEventCard snipe1
    , --- seeker/mystic [eote]
      SomeEventCard protectingTheAnirniq2
    , --- rogue/mystic [eote]
      SomeEventCard etherealSlip
    , SomeEventCard etherealSlip2
    , --- rogue/survivor [eote]
      SomeEventCard hitMe
    , --- neutral [eote]
      SomeEventCard callForBackup2
    , -- The Scarlet Keys
      --- signature [tsk]
      SomeEventCard wordOfWoe
    , SomeEventCard wordOfWeal
    , --- guardian [tsk]
      SomeEventCard customModifications
    , SomeEventCard bolas
    , SomeEventCard breachTheDoor
    , SomeEventCard grievousWound
    , SomeEventCard motivationalSpeech
    , SomeEventCard oneInTheChamber
    , SomeEventCard preparedForTheWorst2
    , SomeEventCard everVigilant4
    , --- seeker [tsk]
      SomeEventCard theRavenQuill
    , SomeEventCard bizarreDiagnosis
    , SomeEventCard captivatingDiscovery
    , SomeEventCard mapTheArea
    , SomeEventCard existentialRiddle1
    , SomeEventCard guidance1
    , --- rogue [tsk]
      SomeEventCard friendsInLowPlaces
    , SomeEventCard honedInstinct
    , SomeEventCard hiddenPocket
    , SomeEventCard hitAndRun
    , SomeEventCard illTakeThat
    , SomeEventCard kickingTheHornetsNest
    , SomeEventCard quickGetaway
    , SomeEventCard breakingAndEntering2
    , SomeEventCard cleanSneak4
    , --- mystic [tsk]
      SomeEventCard powerWord
    , SomeEventCard eldritchInitiation
    , SomeEventCard explosiveWard
    , SomeEventCard stringOfCurses
    , SomeEventCard moonlightRitual2
    , SomeEventCard uncageTheSoul3
    , --- survivor [tsk]
      SomeEventCard makeshiftTrap
    , SomeEventCard endOfTheRoad
    , SomeEventCard exploitWeakness
    , SomeEventCard makingPreparations
    , SomeEventCard predatorOrPrey
    , SomeEventCard shedALight
    , SomeEventCard atACrossroads1
    , SomeEventCard lifeline1
    , SomeEventCard natureOfTheBeast1
    , SomeEventCard heedTheDream2
    , SomeEventCard salvage2
    , SomeEventCard fickleFortune3
    , --- neutral [tsk]
      SomeEventCard refine
    , --- basic weakness [tsk]
      SomeEventCard quantumParadox
    , SomeEventCard payYourDue
    , SomeEventCard underprepared
    , -- The Feast of Hemloch Vale
      --- signature [fhv]
      SomeEventCard adHoc
    , SomeEventCard aethericCurrentYuggoth
    , SomeEventCard aethericCurrentYoth
    , SomeEventCard beguile
    , SomeEventCard stouthearted
    , --- guardian [fhv]
      SomeEventCard absolution
    , SomeEventCard guidedByFaith
    , SomeEventCard holdUp
    , SomeEventCard taskForce
    , SomeEventCard tinker
    , SomeEventCard handEyeCoordination1
    , SomeEventCard secondWind2
    , SomeEventCard flurryOfBlows5
    , SomeEventCard miracleWish5
    , --- seeker [fhv]
      SomeEventCard uncannyGrowth
    , SomeEventCard controlVariable
    , SomeEventCard fineTuning1
    , --- rogue [fhv]
      SomeEventCard bankJob
    , SomeEventCard falseSurrender
    , SomeEventCard grift
    , SomeEventCard vamp
    , SomeEventCard snitch2
    , SomeEventCard vamp3
    , --- mystic [fhv]
      SomeEventCard abyssalRot
    , SomeEventCard aemberRot
    , SomeEventCard putrescentRot
    , SomeEventCard scarletRot
    , SomeEventCard virescentRot
    , SomeEventCard readTheSigns2
    , SomeEventCard spectralRazor2
    , SomeEventCard sealOfTheElders5
    , --- survivor [fhv]
      SomeEventCard pushedToTheLimit
    , SomeEventCard wrongPlaceRightTime
    , SomeEventCard keepFaith2
    , -- Return to Night of the Zealot
      --- guardian [rtnotz]
      SomeEventCard dynamiteBlast2
    , --- seeker [rtnotz]
      SomeEventCard barricade3
    , --- rogue [rtnotz]
      SomeEventCard hotStreak2
    , --- mystic [rtnotz]
      SomeEventCard mindWipe3
    , -- Return to the Dunwich Legacy
      --- seeker [rtdwl]
      SomeEventCard preposterousSketches2
    , --- rogue [rtdwl]
      SomeEventCard contraband2
    , SomeEventCard thinkOnYourFeet2
    , --- survivor [rtdwl]
      SomeEventCard oops2
    , -- Return to the Path to Carcosa
      --- guardian [rtptc]
      SomeEventCard eatLead
    , --- seeker [rtptc]
      SomeEventCard logicalReasoning4
    , --- mystic [rtptc]
      SomeEventCard stormOfSpirits3
    , -- Return to the Forgotten Age
      --- guardian [rttfa]
      SomeEventCard bloodEclipse1
    , --- seeker [rttfa]
      SomeEventCard truthFromFiction2
    , --- survivor [rttfa]
      SomeEventCard alterFate1
    , -- Return to the Circle Undone
      --- survivor [rttcu]
      SomeEventCard trialByFire3
    , -- Investigator Starter Decks
      --- Nathaniel Cho
      SomeEventCard cleanThemOut
    , SomeEventCard counterpunch
    , SomeEventCard getOverHere
    , SomeEventCard glory
    , SomeEventCard monsterSlayer
    , SomeEventCard oneTwoPunch
    , SomeEventCard standTogether
    , SomeEventCard evidence1
    , SomeEventCard galvanize1
    , SomeEventCard counterpunch2
    , SomeEventCard getOverHere2
    , SomeEventCard lessonLearned2
    , SomeEventCard manoAMano2
    , SomeEventCard dynamiteBlast3
    , SomeEventCard taunt3
    , SomeEventCard oneTwoPunch5
    , --- Harvel Walters
      SomeEventCard burningTheMidnightOil
    , SomeEventCard crypticWritings
    , SomeEventCard extensiveResearch
    , SomeEventCard occultInvocation
    , SomeEventCard glimpseTheUnthinkable1
    , SomeEventCard crypticWritings2
    , SomeEventCard iveGotAPlan2
    , SomeEventCard mindOverMatter2
    , SomeEventCard seekingAnswers2
    , -- Winifred Habbamock
      SomeEventCard pilfer
    , SomeEventCard sneakBy
    , SomeEventCard daringManeuver2
    , SomeEventCard cheapShot2
    , SomeEventCard slipAway2
    , SomeEventCard pilfer3
    , SomeEventCard backstab3
    , --- Jacqueline Fine
      SomeEventCard parallelFates
    , SomeEventCard voiceOfRa
    , SomeEventCard eldritchInspiration1
    , SomeEventCard hypnoticGaze2
    , SomeEventCard recharge4
    , --- Stella Clark
      SomeEventCard willToSurvive
    , SomeEventCard aTestOfWill
    , SomeEventCard gritYourTeeth
    , SomeEventCard aTestOfWill2
    , SomeEventCard lookWhatIFound2
    , SomeEventCard dumbLuck2
    , SomeEventCard lucky3
    , -- Promo
      -- The Dirge of Reason
      SomeEventCard mysteriesRemain
    ]
