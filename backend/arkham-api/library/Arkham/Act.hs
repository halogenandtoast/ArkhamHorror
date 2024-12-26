{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Act (
  Act (..),
  lookupAct,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Act.Acts
import Arkham.Act.Types
import Arkham.Card
import Arkham.Id
import Control.Monad.Fail (fail)

data MissingAct = MissingAct CardCode
  deriving stock (Show, Eq)

instance Exception MissingAct

lookupAct :: ActId -> Int -> CardId -> Either MissingAct Act
lookupAct actId = case lookup (unActId actId) allActs of
  Nothing -> \_ _ -> Left $ MissingAct (coerce actId)
  Just (SomeActCard a) -> \i cardId -> Right . Act $ cbCardBuilder a cardId (i, actId)

instance FromJSON Act where
  parseJSON = withObject "Act" $ \o -> do
    cCode <- o .: "id"
    case withActCardCode cCode (\(_ :: ActCard a) -> Act <$> parseJSON @a (Object o)) of
      Left (MissingAct c) -> fail $ "Unknown act: " <> show c
      Right a -> a

withActCardCode :: CardCode -> (forall a. IsAct a => ActCard a -> r) -> Either MissingAct r
withActCardCode cCode f = case lookup cCode allActs of
  Nothing -> Left $ MissingAct cCode
  Just (SomeActCard a) -> Right (f a)

allActs :: Map CardCode SomeActCard
allActs =
  mapFrom
    someActCardCode
    [ -- Night of the Zealot
      -- The Gathering
      SomeActCard trapped
    , SomeActCard theBarrier
    , SomeActCard whatHaveYouDone
    , -- The Midnight Masks
      SomeActCard uncoveringTheConspiracy
    , -- The Devourer Below
      SomeActCard investigatingTheTrail
    , SomeActCard intoTheDarkness
    , SomeActCard disruptingTheRitual
    , -- The Dunwich Legacy
      -- Extracurricular Activity
      SomeActCard afterHours
    , SomeActCard ricesWhereabouts
    , SomeActCard campusSafety
    , -- The House Always WIns
      SomeActCard beginnersLuck
    , SomeActCard skinGame
    , SomeActCard allIn
    , SomeActCard fold
    , -- The Miskatonic Museum
      SomeActCard findingAWayInside
    , SomeActCard nightAtTheMuseum
    , SomeActCard breakingAndEntering
    , SomeActCard searchingForTheTome
    , -- The Essex County Express
      SomeActCard run
    , SomeActCard getTheEngineRunning
    , -- Blood on the Altar
      SomeActCard searchingForAnswers
    , SomeActCard theChamberOfTheBeast
    , -- Undimensioned and Unseen
      SomeActCard saracenicScript
    , SomeActCard theyMustBeDestroyed
    , -- Where Doom Awaits
      SomeActCard thePathToTheHill
    , SomeActCard ascendingTheHillV1
    , SomeActCard ascendingTheHillV2
    , SomeActCard ascendingTheHillV3
    , SomeActCard theGateOpens
    , -- Lost in Time and Space
      SomeActCard outOfThisWorld
    , SomeActCard intoTheBeyond
    , SomeActCard closeTheRift
    , SomeActCard findingANewWay
    , -- The Path to Carcosa
      -- Curtain Call
      SomeActCard awakening
    , SomeActCard theStrangerACityAflame
    , SomeActCard theStrangerThePathIsMine
    , SomeActCard theStrangerTheShoresOfHali
    , SomeActCard curtainCall
    , -- The Last King
      SomeActCard discoveringTheTruth
    , -- Echoes of the Past
      SomeActCard raceForAnswers
    , SomeActCard mistakesOfThePast
    , SomeActCard theOath
    , -- The Unspeakable Oath
      SomeActCard arkhamAsylum
    , SomeActCard theReallyBadOnesV1
    , SomeActCard theReallyBadOnesV2
    , SomeActCard planningTheEscape
    , SomeActCard noAsylum
    , -- A Phantom of Truth
      SomeActCard theParisianConspiracyV1
    , SomeActCard theParisianConspiracyV2
    , SomeActCard pursuingShadows
    , SomeActCard stalkedByShadows
    , -- The Pallid Mask
      SomeActCard throughTheCatacombs
    , SomeActCard thePathIsBarred
    , SomeActCard theWayOut
    , SomeActCard leadingTheWay
    , -- Black Stars Rise
      SomeActCard openThePathBelow
    , SomeActCard openThePathAbove
    , -- Dim Carcosa
      SomeActCard inLostCarcosa
    , SomeActCard searchForTheStrangerV1
    , SomeActCard searchForTheStrangerV2
    , SomeActCard searchForTheStrangerV3
    , SomeActCard theKingInTatters
    , -- The Forgotten Age
      -- The Untamed Wilds
      SomeActCard exploringTheRainforest
    , SomeActCard huntressOfTheEztli
    , SomeActCard searchForTheRuins
    , SomeActCard theGuardedRuins
    , -- The Doom of Eztli
      SomeActCard intoTheRuins
    , SomeActCard magicAndScience
    , SomeActCard escapeTheRuins
    , -- Threads of Fate
      SomeActCard theRelicIsMissing
    , SomeActCard harlanIsInDanger
    , SomeActCard atTheExhibitTheRelicsLocation
    , SomeActCard atTheExhibitTheBrotherhoodsPlot
    , SomeActCard harlansCurseSafekeeping
    , SomeActCard harlansCurseHarlanEarnstone
    , SomeActCard findTheRelic
    , SomeActCard recoverTheRelic
    , SomeActCard searchForAlejandro
    , SomeActCard missingPersons
    , SomeActCard atTheStationInShadowedTalons
    , SomeActCard atTheStationTrainTracks
    , SomeActCard friendsInHighPlacesHenrysInformation
    , SomeActCard friendsInHighPlacesHenryDeveau
    , SomeActCard alejandrosPrison
    , SomeActCard alejandrosPlight
    , SomeActCard trialOfTheHuntress
    , SomeActCard theGuardiansInquiry
    , SomeActCard theCaveOfDarknessEmbroiledInBattle
    , SomeActCard theCaveOfDarknessTunnelsInTheDark
    , SomeActCard strangeRelicsMariaDeSilva
    , SomeActCard strangeRelicsMariasInformation
    , SomeActCard strangeOccurences
    , SomeActCard theBrotherhoodIsRevealed
    , -- The Boundary Beyond
      SomeActCard crossingTheThreshold
    , SomeActCard pastAndPresent
    , SomeActCard theReturnTrip
    , -- Heart of the Elders
      --- Pillars of Judgement
      SomeActCard searchForThePattern
    , SomeActCard openingTheMaw
    , --- K'n-yan
      SomeActCard cavernOfTheForgottenAge
    , SomeActCard descentIntoDark
    , -- The City of Archives
      SomeActCard exploringPnakotus
    , SomeActCard restrictedAccess
    , SomeActCard repossession
    , -- The Depths of Yoth
      SomeActCard journeyToTheNexus
    , -- Shattered Aeons
      SomeActCard worldsBeyond
    , SomeActCard searchForTheBrotherhood
    , SomeActCard theYithianRelic
    , SomeActCard mendTheShatter
    , SomeActCard paradiseLost
    , SomeActCard timelock
    , --- Turn Back Time
      SomeActCard intoTheRuinsOnceAgain
    , SomeActCard theChamberOfStillRemains
    , SomeActCard momentOfDoom
    , -- The Circle Undone
      --- Disappearance at the Twilight Estate
      SomeActCard theDisappearance
    , --- The Witching Hour
      SomeActCard lostInTheWoods
    , SomeActCard witchHauntings
    , SomeActCard pathsIntoTwilight
    , SomeActCard aCircleUnbroken
    , --- At Death's Doorstep
      SomeActCard hiddenAgendas
    , SomeActCard theSpectralRealm
    , SomeActCard escapeTheCage
    , --- The Secret Name
      SomeActCard investigatingTheWitchHouse
    , SomeActCard beyondTheWitchHouse
    , SomeActCard stoppingTheRitual
    , --- The Wages of Sin
      SomeActCard inPursuitOfTheDead
    , SomeActCard inPursuitOfTheLiving
    , --- For the Greater Good
      SomeActCard warmWelcome
    , SomeActCard infiltratingTheLodge
    , SomeActCard obtainingTheDevice
    , SomeActCard theFourKeys
    , -- Union and Disillusion
      SomeActCard theUnvisitedIsle
    , SomeActCard fatedSouls
    , SomeActCard beyondTheMistV1
    , SomeActCard beyondTheMistV2
    , SomeActCard beyondTheMistV3
    , SomeActCard beyondTheMistV4
    , SomeActCard theBindingRite
    , SomeActCard theBrokenRite
    , -- In the Clutches of Chaos
      SomeActCard darkKnowledgeV1
    , SomeActCard beyondTheGrave
    , SomeActCard darkKnowledgeV2
    , SomeActCard newWorldOrder
    , -- Before the Black Throne
      SomeActCard theCosmosBeckons
    , SomeActCard inAzathothsDomain
    , SomeActCard whatMustBeDone
    , -- The Dream-Eaters
      --- Beyond the Gates of Sleep
      SomeActCard enteringTheDreamlands
    , SomeActCard theTrialOfNashtAndKamanThah
    , SomeActCard theFinalDescent
    , SomeActCard thePath
    , --- Waking Nightmare
      SomeActCard lookingForAnswers
    , SomeActCard searchForThePatient
    , SomeActCard containingTheOutbreak
    , --- The Search for Kadath
      SomeActCard kingdomOfTheSkai
    , SomeActCard theIsleOfOriab
    , SomeActCard theDoomThatCameBefore
    , SomeActCard seekOutTheNight
    , SomeActCard theKingsDecree
    , --- A Thousand Shapes of Horror
      SomeActCard searchingTheUnnamable
    , SomeActCard theEndlessStairs
    , --- Dark Side of the Moon
      SomeActCard inTheBellyOfTheMoonBeast
    , SomeActCard exploringTheMoon
    , SomeActCard theMoonsCore
    , SomeActCard unexpectedRescue
    , --- Point of No Return
      SomeActCard enteringTheUnderworldV1
    , SomeActCard enteringTheUnderworldV2
    , SomeActCard theDescent
    , SomeActCard theBlackExpanse
    , --- Where the Gods Dwell
      SomeActCard journeyThroughTheColdWastes
    , SomeActCard theThingInTheRobes
    , SomeActCard beyondDreams
    , SomeActCard truthAndLies
    , SomeActCard theDreamEaters
    , --- Weaver of the Cosmos
      SomeActCard journeyAcrossTheBridge
    , SomeActCard theWeaverOfTheCosmos
    , SomeActCard theSchemesDemise
    , -- The Innsmouth Conspiracy
      --- The Pit of Despair
      SomeActCard thePit
    , SomeActCard theEscape
    , --- The Vanishing of Elina Harper
      SomeActCard theSearchForAgentHarper
    , SomeActCard theRescue
    , --- In Too Deep
      SomeActCard throughTheLabyrinth
    , --- Devil Reef
      SomeActCard reefOfMysteries
    , --- Horror in High Gear
      SomeActCard pedalToTheMetal
    , --- A Light in the Fog
      SomeActCard theLighthouse
    , SomeActCard findingThePath
    , SomeActCard worshippersOfTheDeep
    , --- The Lair of Dagon
      SomeActCard theFirstOath
    , SomeActCard theSecondOath
    , SomeActCard theThirdOath
    , --- Into the Maelstrom
      SomeActCard backIntoTheDepths
    , SomeActCard cityOfTheDeepV1
    , SomeActCard cityOfTheDeepV2
    , SomeActCard cityOfTheDeepV3
    , -- Edge of the Earth
      --- The Crash [eote]
      SomeActCard searchForACampSite
    , --- Lost in the Night [eote]
      SomeActCard theLostExpedition
    , --- Seeping Nightmares [eote]
      SomeActCard underAttack
    , --- Fatal Mirage [eote]
      SomeActCard shadowOfThePastV1
    , SomeActCard shadowOfThePastV2
    , SomeActCard shadowOfThePastV3
    , --- To the Forbidden Peaks [eote]
      SomeActCard ascendTheMountain
    , -- Return to Night of the Zealot
      -- Return to the Gathering
      SomeActCard mysteriousGateway
    , -- Curse of the Rougarou
      SomeActCard findingLadyEsprit
    , SomeActCard huntingTheRougarou
    , -- Carnevale of Horrors
      SomeActCard theCarnevaleConspiracy
    , SomeActCard getToTheBoats
    , SomeActCard row
    , -- Murder at the Excelsior Hotel
      SomeActCard whatHappened
    , SomeActCard followingLeads
    ]
