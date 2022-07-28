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
lookupAct actId = case lookup (unActId actId) allActs of
  Nothing -> error $ "Unknown act: " <> show actId
  Just (SomeActCard a) -> \i -> Act $ cbCardBuilder a (i, actId)

instance FromJSON Act where
  parseJSON v = flip (withObject "Act") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    withActCardCode cCode $ \(_ :: ActCard a) -> Act <$> parseJSON @a v

withActCardCode
  :: CardCode
  -> (forall a. IsAct a => ActCard a -> r)
  -> r
withActCardCode cCode f =
  case lookup cCode allActs of
    Nothing -> error $ "Unknown act: " <> show cCode
    Just (SomeActCard a) -> f a

data SomeActCard = forall a. IsAct a => SomeActCard (ActCard a)

liftSomeActCard :: (forall a. ActCard a -> b) -> SomeActCard -> b
liftSomeActCard f (SomeActCard a) = f a

someActCardCode :: SomeActCard -> CardCode
someActCardCode = liftSomeActCard cbCardCode

allActs :: HashMap CardCode SomeActCard
allActs = mapFromList $ map
  (toFst someActCardCode)
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
