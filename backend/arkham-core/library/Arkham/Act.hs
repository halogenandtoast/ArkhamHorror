module Arkham.Act (
  Act (..),
  lookupAct,
) where

import Arkham.Prelude hiding (fold)

import Arkham.Act.Acts
import Arkham.Act.Attrs
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Data.Typeable

data Act = forall a. IsAct a => Act a

instance Eq Act where
  (Act (a :: a)) == (Act (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Act where
  show (Act a) = show a

instance ToJSON Act where
  toJSON (Act a) = toJSON a

instance HasAbilities Act where
  getAbilities (Act a) = getAbilities a

instance RunMessage Act where
  runMessage msg (Act a) = Act <$> runMessage msg a

instance HasModifiersFor Act where
  getModifiersFor source target (Act a) = getModifiersFor source target a

instance Entity Act where
  type EntityId Act = ActId
  type EntityAttrs Act = ActAttrs
  toId = toId . toAttrs
  toAttrs (Act a) = toAttrs a
  overAttrs f (Act a) = Act $ overAttrs f a

instance TargetEntity Act where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Act where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupAct :: ActId -> (Int -> Act)
lookupAct actId =
  fromJustNote ("Unknown act: " <> show actId) $ lookup actId allActs

instance FromJSON Act where
  parseJSON v = flip (withObject "Act") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    case cCode of
      -- Night of the Zealot
      -- The Gathering
      "01108" -> Act . Trapped <$> parseJSON v
      "01109" -> Act . TheBarrier <$> parseJSON v
      "01110" -> Act . WhatHaveYouDone <$> parseJSON v
      -- The Midnight Masks
      "01123" -> Act . UncoveringTheConspiracy <$> parseJSON v
      -- The Devourer Below
      "01146" -> Act . InvestigatingTheTrail <$> parseJSON v
      "01147" -> Act . IntoTheDarkness <$> parseJSON v
      "01148" -> Act . DisruptingTheRitual <$> parseJSON v
      -- The Dunwich Legacy
      -- Extracurricular Activity
      "02045" -> Act . AfterHours <$> parseJSON v
      "02046" -> Act . RicesWhereabouts <$> parseJSON v
      "02047" -> Act . CampusSafety <$> parseJSON v
      -- The House Always WIns
      "02066" -> Act . BeginnersLuck <$> parseJSON v
      "02067" -> Act . SkinGame <$> parseJSON v
      "02068" -> Act . AllIn <$> parseJSON v
      "02069" -> Act . Fold <$> parseJSON v
      -- The Miskatonic Museum
      "02122" -> Act . FindingAWayInside <$> parseJSON v
      "02123" -> Act . NightAtTheMuseum <$> parseJSON v
      "02124" -> Act . BreakingAndEntering <$> parseJSON v
      "02125" -> Act . SearchingForTheTome <$> parseJSON v
      -- The Essex County Express
      "02165" -> Act . Run <$> parseJSON v
      "02166" -> Act . GetTheEngineRunning <$> parseJSON v
      -- Blood on the Altar
      "02199" -> Act . SearchingForAnswers <$> parseJSON v
      "02200" -> Act . TheChamberOfTheBeast <$> parseJSON v
      -- Undimensioned and Unseen
      "02240" -> Act . SaracenicScript <$> parseJSON v
      "02241" -> Act . TheyMustBeDestroyed <$> parseJSON v
      -- Where Doom Awaits
      "02277" -> Act . ThePathToTheHill <$> parseJSON v
      "02278" -> Act . AscendingTheHillV1 <$> parseJSON v
      "02279" -> Act . AscendingTheHillV2 <$> parseJSON v
      "02280" -> Act . AscendingTheHillV3 <$> parseJSON v
      "02281" -> Act . TheGateOpens <$> parseJSON v
      -- Lost in Time and Space
      "02316" -> Act . OutOfThisWorld <$> parseJSON v
      "02317" -> Act . IntoTheBeyond <$> parseJSON v
      "02318" -> Act . CloseTheRift <$> parseJSON v
      "02319" -> Act . FindingANewWay <$> parseJSON v
      -- The Path to Carcosa
      -- Curtain Call
      "03046" -> Act . Awakening <$> parseJSON v
      "03047a" -> Act . TheStrangerACityAflame <$> parseJSON v
      "03047b" -> Act . TheStrangerThePathIsMine <$> parseJSON v
      "03047c" -> Act . TheStrangerTheShoresOfHali <$> parseJSON v
      "03048" -> Act . CurtainCall <$> parseJSON v
      -- The Last King
      "03064" -> Act . DiscoveringTheTruth <$> parseJSON v
      -- Echoes of the Past
      "03124" -> Act . RaceForAnswers <$> parseJSON v
      "03125" -> Act . MistakesOfThePast <$> parseJSON v
      "03126" -> Act . TheOath <$> parseJSON v
      -- The Unspeakable Oath
      "03163" -> Act . ArkhamAsylum <$> parseJSON v
      "03164" -> Act . TheReallyBadOnesV1 <$> parseJSON v
      "03165" -> Act . TheReallyBadOnesV2 <$> parseJSON v
      "03166" -> Act . PlanningTheEscape <$> parseJSON v
      "03167" -> Act . NoAsylum <$> parseJSON v
      -- A Phantom of Truth
      "03204" -> Act . TheParisianConspiracyV1 <$> parseJSON v
      "03205" -> Act . TheParisianConspiracyV2 <$> parseJSON v
      "03206" -> Act . PursuingShadows <$> parseJSON v
      "03207" -> Act . StalkedByShadows <$> parseJSON v
      -- The Pallid Mask
      "03243" -> Act . ThroughTheCatacombs <$> parseJSON v
      "03244" -> Act . ThePathIsBarred <$> parseJSON v
      "03245" -> Act . TheWayOut <$> parseJSON v
      "03246" -> Act . LeadingTheWay <$> parseJSON v
      -- Black Stars Rise
      "03281" -> Act . OpenThePathBelow <$> parseJSON v
      "03282" -> Act . OpenThePathAbove <$> parseJSON v
      -- Dim Carcosa
      "03320" -> Act . InLostCarcosa <$> parseJSON v
      "03321" -> Act . SearchForTheStrangerV1 <$> parseJSON v
      "03322" -> Act . SearchForTheStrangerV2 <$> parseJSON v
      "03323" -> Act . SearchForTheStrangerV3 <$> parseJSON v
      "03324" -> Act . TheKingInTatters <$> parseJSON v
      -- The Forgotten Age
      -- The Untamed Wilds
      "04046" -> Act . ExploringTheRainforest <$> parseJSON v
      "04047" -> Act . HuntressOfTheEztli <$> parseJSON v
      "04048" -> Act . SearchForTheRuins <$> parseJSON v
      "04049" -> Act . TheGuardedRuins <$> parseJSON v
      -- Return to Night of the Zealot
      -- Return to the Gathering
      "50012" -> Act . MysteriousGateway <$> parseJSON v
      -- Curse of the Rougarou
      "81005" -> Act . FindingLadyEsprit <$> parseJSON v
      "81006" -> Act . HuntingTheRougarou <$> parseJSON v
      -- Carnevale of Horrors
      "82005" -> Act . TheCarnevaleConspiracy <$> parseJSON v
      "82006" -> Act . GetToTheBoats <$> parseJSON v
      "82007" -> Act . Row <$> parseJSON v
      _ -> error "invalid act"

allActs :: HashMap ActId (Int -> Act)
allActs = mapFromList $ map
  (\cb -> (ActId (cbCardCode cb), \deckId -> cbCardBuilder cb (deckId, ActId (cbCardCode cb))))
  [ -- Night of the Zealot
  -- The Gathering
    Act <$> trapped
  , Act <$> theBarrier
  , Act <$> whatHaveYouDone
  -- The Midnight Masks
  , Act <$> uncoveringTheConspiracy
  -- The Devourer Below
  , Act <$> investigatingTheTrail
  , Act <$> intoTheDarkness
  , Act <$> disruptingTheRitual
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , Act <$> afterHours
  , Act <$> ricesWhereabouts
  , Act <$> campusSafety
  -- The House Always WIns
  , Act <$> beginnersLuck
  , Act <$> skinGame
  , Act <$> allIn
  , Act <$> fold
  -- The Miskatonic Museum
  , Act <$> findingAWayInside
  , Act <$> nightAtTheMuseum
  , Act <$> breakingAndEntering
  , Act <$> searchingForTheTome
  -- The Essex County Express
  , Act <$> run
  , Act <$> getTheEngineRunning
  -- Blood on the Altar
  , Act <$> searchingForAnswers
  , Act <$> theChamberOfTheBeast
  -- Undimensioned and Unseen
  , Act <$> saracenicScript
  , Act <$> theyMustBeDestroyed
  -- Where Doom Awaits
  , Act <$> thePathToTheHill
  , Act <$> ascendingTheHillV1
  , Act <$> ascendingTheHillV2
  , Act <$> ascendingTheHillV3
  , Act <$> theGateOpens
  -- Lost in Time and Space
  , Act <$> outOfThisWorld
  , Act <$> intoTheBeyond
  , Act <$> closeTheRift
  , Act <$> findingANewWay
  -- The Path to Carcosa
  -- Curtain Call
  , Act <$> awakening
  , Act <$> theStrangerACityAflame
  , Act <$> theStrangerThePathIsMine
  , Act <$> theStrangerTheShoresOfHali
  , Act <$> curtainCall
  -- The Last King
  , Act <$> discoveringTheTruth
  -- Echoes of the Past
  , Act <$> raceForAnswers
  , Act <$> mistakesOfThePast
  , Act <$> theOath
  -- The Unspeakable Oath
  , Act <$> arkhamAsylum
  , Act <$> theReallyBadOnesV1
  , Act <$> theReallyBadOnesV2
  , Act <$> planningTheEscape
  , Act <$> noAsylum
  -- A Phantom of Truth
  , Act <$> theParisianConspiracyV1
  , Act <$> theParisianConspiracyV2
  , Act <$> pursuingShadows
  , Act <$> stalkedByShadows
  -- The Pallid Mask
  , Act <$> throughTheCatacombs
  , Act <$> thePathIsBarred
  , Act <$> theWayOut
  , Act <$> leadingTheWay
  -- Black Stars Rise
  , Act <$> openThePathBelow
  , Act <$> openThePathAbove
  -- Dim Carcosa
  , Act <$> inLostCarcosa
  , Act <$> searchForTheStrangerV1
  , Act <$> searchForTheStrangerV2
  , Act <$> searchForTheStrangerV3
  , Act <$> theKingInTatters
  -- The Forgotten Age
  -- The Untamed Wilds
  , Act <$> exploringTheRainforest
  , Act <$> huntressOfTheEztli
  , Act <$> searchForTheRuins
  , Act <$> theGuardedRuins
  -- Return to Night of the Zealot
  -- Return to the Gathering
  , Act <$> mysteriousGateway
  -- Curse of the Rougarou
  , Act <$> findingLadyEsprit
  , Act <$> huntingTheRougarou
  -- Carnevale of Horrors
  , Act <$> theCarnevaleConspiracy
  , Act <$> getToTheBoats
  , Act <$> row
  ]
