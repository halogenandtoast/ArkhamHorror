{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Act
  ( Act(..)
  , lookupAct
  ) where

import Arkham.Prelude hiding ( fold )

import Arkham.Act.Acts
import Arkham.Act.Types
import Arkham.Card
import Arkham.Id

lookupAct :: ActId -> (Int -> Act)
lookupAct actId = case lookup (unActId actId) allActs of
  Nothing -> error $ "Unknown act: " <> show actId
  Just (SomeActCard a) -> \i -> Act $ cbCardBuilder a (i, actId)

instance FromJSON Act where
  parseJSON = withObject "Act" $ \o -> do
    cCode <- o .: "id"
    withActCardCode cCode $ \(_ :: ActCard a) -> Act <$> parseJSON @a (Object o)

withActCardCode :: CardCode -> (forall a . IsAct a => ActCard a -> r) -> r
withActCardCode cCode f = case lookup cCode allActs of
  Nothing -> error $ "Unknown act: " <> show cCode
  Just (SomeActCard a) -> f a

allActs :: HashMap CardCode SomeActCard
allActs = mapFrom
  someActCardCode
  [ -- Night of the Zealot
  -- The Gathering
    SomeActCard trapped
  , SomeActCard theBarrier
  , SomeActCard whatHaveYouDone
  -- The Midnight Masks
  , SomeActCard uncoveringTheConspiracy
  -- The Devourer Below
  , SomeActCard investigatingTheTrail
  , SomeActCard intoTheDarkness
  , SomeActCard disruptingTheRitual
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , SomeActCard afterHours
  , SomeActCard ricesWhereabouts
  , SomeActCard campusSafety
  -- The House Always WIns
  , SomeActCard beginnersLuck
  , SomeActCard skinGame
  , SomeActCard allIn
  , SomeActCard fold
  -- The Miskatonic Museum
  , SomeActCard findingAWayInside
  , SomeActCard nightAtTheMuseum
  , SomeActCard breakingAndEntering
  , SomeActCard searchingForTheTome
  -- The Essex County Express
  , SomeActCard run
  , SomeActCard getTheEngineRunning
  -- Blood on the Altar
  , SomeActCard searchingForAnswers
  , SomeActCard theChamberOfTheBeast
  -- Undimensioned and Unseen
  , SomeActCard saracenicScript
  , SomeActCard theyMustBeDestroyed
  -- Where Doom Awaits
  , SomeActCard thePathToTheHill
  , SomeActCard ascendingTheHillV1
  , SomeActCard ascendingTheHillV2
  , SomeActCard ascendingTheHillV3
  , SomeActCard theGateOpens
  -- Lost in Time and Space
  , SomeActCard outOfThisWorld
  , SomeActCard intoTheBeyond
  , SomeActCard closeTheRift
  , SomeActCard findingANewWay
  -- The Path to Carcosa
  -- Curtain Call
  , SomeActCard awakening
  , SomeActCard theStrangerACityAflame
  , SomeActCard theStrangerThePathIsMine
  , SomeActCard theStrangerTheShoresOfHali
  , SomeActCard curtainCall
  -- The Last King
  , SomeActCard discoveringTheTruth
  -- Echoes of the Past
  , SomeActCard raceForAnswers
  , SomeActCard mistakesOfThePast
  , SomeActCard theOath
  -- The Unspeakable Oath
  , SomeActCard arkhamAsylum
  , SomeActCard theReallyBadOnesV1
  , SomeActCard theReallyBadOnesV2
  , SomeActCard planningTheEscape
  , SomeActCard noAsylum
  -- A Phantom of Truth
  , SomeActCard theParisianConspiracyV1
  , SomeActCard theParisianConspiracyV2
  , SomeActCard pursuingShadows
  , SomeActCard stalkedByShadows
  -- The Pallid Mask
  , SomeActCard throughTheCatacombs
  , SomeActCard thePathIsBarred
  , SomeActCard theWayOut
  , SomeActCard leadingTheWay
  -- Black Stars Rise
  , SomeActCard openThePathBelow
  , SomeActCard openThePathAbove
  -- Dim Carcosa
  , SomeActCard inLostCarcosa
  , SomeActCard searchForTheStrangerV1
  , SomeActCard searchForTheStrangerV2
  , SomeActCard searchForTheStrangerV3
  , SomeActCard theKingInTatters
  -- The Forgotten Age
  -- The Untamed Wilds
  , SomeActCard exploringTheRainforest
  , SomeActCard huntressOfTheEztli
  , SomeActCard searchForTheRuins
  , SomeActCard theGuardedRuins
  -- The Doom of Eztli
  , SomeActCard intoTheRuins
  , SomeActCard magicAndScience
  , SomeActCard escapeTheRuins
  -- Threads of Fate
  , SomeActCard theRelicIsMissing
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
  -- The Boundary Beyond
  , SomeActCard crossingTheThreshold
  , SomeActCard pastAndPresent
  , SomeActCard theReturnTrip
  -- Heart of the Elders
  --- Pillars of Judgement
  , SomeActCard searchForThePattern
  , SomeActCard openingTheMaw
  --- K'n-yan
  , SomeActCard cavernOfTheForgottenAge
  , SomeActCard descentIntoDark
  -- The City of Archives
  , SomeActCard exploringPnakotus
  , SomeActCard restrictedAccess
  , SomeActCard repossession
  -- The Depths of Yoth
  , SomeActCard journeyToTheNexus
  -- Shattered Aeons
  , SomeActCard worldsBeyond
  , SomeActCard searchForTheBrotherhood
  , SomeActCard theYithianRelic
  , SomeActCard mendTheShatter
  , SomeActCard paradiseLost
  , SomeActCard timelock
  --- Turn Back Time
  , SomeActCard intoTheRuinsOnceAgain
  , SomeActCard theChamberOfStillRemains
  , SomeActCard momentOfDoom
  -- The Circle Undone
  --- Disappearance at the Twilight Estate
  , SomeActCard theDisappearance
  -- Return to Night of the Zealot
  -- Return to the Gathering
  , SomeActCard mysteriousGateway
  -- Curse of the Rougarou
  , SomeActCard findingLadyEsprit
  , SomeActCard huntingTheRougarou
  -- Carnevale of Horrors
  , SomeActCard theCarnevaleConspiracy
  , SomeActCard getToTheBoats
  , SomeActCard row
  ]
