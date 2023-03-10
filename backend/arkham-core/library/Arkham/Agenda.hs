{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Agenda
  ( module Arkham.Agenda
  ) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Id

lookupAgenda :: AgendaId -> Int -> Agenda
lookupAgenda agendaId = case lookup (unAgendaId agendaId) allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show agendaId
  Just (SomeAgendaCard a) -> \i -> Agenda $ cbCardBuilder a (i, agendaId)

instance RunMessage Agenda where
  runMessage msg (Agenda a) = Agenda <$> runMessage msg a

instance FromJSON Agenda where
  parseJSON = withObject "Agenda" $ \o -> do
    cCode <- o .: "id"
    withAgendaCardCode cCode
      $ \(_ :: AgendaCard a) -> Agenda <$> parseJSON @a (Object o)

withAgendaCardCode
  :: CardCode -> (forall a . IsAgenda a => AgendaCard a -> r) -> r
withAgendaCardCode cCode f = case lookup cCode allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show cCode
  Just (SomeAgendaCard a) -> f a

allAgendas :: HashMap CardCode SomeAgendaCard
allAgendas = mapFrom
  someAgendaCardCode
  [ -- Night of the Zealot
  -- The Gathering
    SomeAgendaCard whatsGoingOn
  , SomeAgendaCard riseOfTheGhouls
  , SomeAgendaCard theyreGettingOut
  -- The Midnight Masks
  , SomeAgendaCard predatorOrPrey
  , SomeAgendaCard timeIsRunningShort
  -- The Devourer Below
  , SomeAgendaCard theArkhamWoods
  , SomeAgendaCard theRitualBegins
  , SomeAgendaCard vengeanceAwaits
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , SomeAgendaCard quietHalls
  , SomeAgendaCard deadOfNight
  , SomeAgendaCard theBeastUnleashed
  -- The House Always Wins
  , SomeAgendaCard theCloverClub
  , SomeAgendaCard undergroundMuscle
  , SomeAgendaCard chaosInTheCloverClub
  -- The Miskatonic Museum
  , SomeAgendaCard restrictedAccess
  , SomeAgendaCard shadowsDeepen
  , SomeAgendaCard inEveryShadow
  -- The Essex County Express
  , SomeAgendaCard aTearInReality
  , SomeAgendaCard theMawWidens
  , SomeAgendaCard rollingBackwards
  , SomeAgendaCard drawnIn
  , SomeAgendaCard outOfTime
  -- Blood on the Altar
  , SomeAgendaCard strangeDisappearances
  , SomeAgendaCard theOldOnesHunger
  , SomeAgendaCard feedTheBeast
  -- Undimensioned and Unseen
  , SomeAgendaCard rampagingCreatures
  , SomeAgendaCard bidingItsTime
  , SomeAgendaCard horrorsUnleashed
  -- Where Doom Awaits
  , SomeAgendaCard callingForthTheOldOnes
  , SomeAgendaCard beckoningForPower
  -- Lost in Time and Space
  , SomeAgendaCard allIsOne
  , SomeAgendaCard pastPresentAndFuture
  , SomeAgendaCard breakingThrough
  , SomeAgendaCard theEndOfAllThings
  -- The Path to Carcosa
  -- Curtain Call
  , SomeAgendaCard theThirdAct
  , SomeAgendaCard encore
  -- The Last King
  , SomeAgendaCard fashionablyLate
  , SomeAgendaCard theTerrifyingTruth
  -- Echoes of the Past
  , SomeAgendaCard theTruthIsHidden
  , SomeAgendaCard ransackingTheManor
  , SomeAgendaCard secretsBetterLeftHidden
  -- The Unspeakable Oath
  , SomeAgendaCard lockedInside
  , SomeAgendaCard torturousDescent
  , SomeAgendaCard hisDomain
  -- A Phantom of Truth
  , SomeAgendaCard theFirstNight
  , SomeAgendaCard theSecondNight
  , SomeAgendaCard theThirdNight
  -- The Pallid Mask
  , SomeAgendaCard empireOfTheDead
  , SomeAgendaCard empireOfTheUndead
  -- Black Stars Rise
  , SomeAgendaCard theTideRises
  , SomeAgendaCard letTheStormRageTheFloodBelow
  , SomeAgendaCard letTheStormRageTheVortexAbove
  , SomeAgendaCard theCityFloods
  , SomeAgendaCard theRitualBeginsBlackStarsRise
  , SomeAgendaCard theEntityAboveTheFloodBelow
  , SomeAgendaCard theEntityAboveTheVortexAbove
  , SomeAgendaCard swallowedSky
  -- Dim Carcosa
  , SomeAgendaCard madnessCoils
  , SomeAgendaCard madnessDrowns
  , SomeAgendaCard madnessDies
  -- The Forgotten Age
  -- The Untamed Wilds
  , SomeAgendaCard expeditionIntoTheWild
  , SomeAgendaCard intruders
  -- The Doom of Eztli
  , SomeAgendaCard somethingStirs
  , SomeAgendaCard theTempleWarden
  -- Threads of Fate
  , SomeAgendaCard threeFates
  , SomeAgendaCard behindTheCurtain
  , SomeAgendaCard hiddenEntanglements
  -- The Boundary Beyond
  , SomeAgendaCard theBoundaryBroken
  , SomeAgendaCard theBarrierIsThin
  , SomeAgendaCard timeCollapsing
  -- Heart of the Elders
  --- Pillars of Judgement
  , SomeAgendaCard theJunglesHeart
  , SomeAgendaCard settingSun
  --- K'n-yan
  , SomeAgendaCard theLonelyCaverns
  , SomeAgendaCard eyesInTheDark
  -- The City of Archives
  , SomeAgendaCard cityOfTheGreatRace
  , SomeAgendaCard lostMemories
  , SomeAgendaCard humanityFading
  -- The Depth of Yoth
  , SomeAgendaCard theDescentBegins
  , SomeAgendaCard horrificDescent
  , SomeAgendaCard endlessCaverns
  , SomeAgendaCard cityOfBlood
  , SomeAgendaCard furyThatShakesTheEarth
  , SomeAgendaCard theRedDepths
  , SomeAgendaCard vengeance
  -- Shattered Aeons
  , SomeAgendaCard threadsOfTime
  , SomeAgendaCard pendolousThreads
  , SomeAgendaCard snappedThreads
  -- The Circle Undone
  -- Disappearance at the Twilight Estate
  , SomeAgendaCard judgementXX
  -- The Witching Hour
  , SomeAgendaCard temperanceXIV
  , SomeAgendaCard theNightHowls
  -- Return to the Night of the Zealot
  -- Return to the Midnight Masks
  , SomeAgendaCard returnToPredatorOrPrey
  -- Curse of the Rougarou
  , SomeAgendaCard aCreatureOfTheBayou
  , SomeAgendaCard theRougarouFeeds
  , SomeAgendaCard theCurseSpreads
  -- Carnevale of Horrors
  , SomeAgendaCard theFestivitiesBegin
  , SomeAgendaCard theShadowOfTheEclipse
  , SomeAgendaCard chaosAtTheCarnevale
  ]
