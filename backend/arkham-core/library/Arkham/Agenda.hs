module Arkham.Agenda (
  module Arkham.Agenda,
) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Data.Typeable

data Agenda = forall a. IsAgenda a => Agenda a

instance Eq Agenda where
  (Agenda (a :: a)) == (Agenda (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Agenda where
  show (Agenda a) = show a

instance ToJSON Agenda where
  toJSON (Agenda a) = toJSON a

lookupAgenda :: AgendaId -> (Int -> Agenda)
lookupAgenda agendaId = case lookup (unAgendaId agendaId) allAgendas of
  Nothing -> error $ "Unknown agenda: " <> show agendaId
  Just (SomeAgendaCard a) -> \i -> Agenda $ cbCardBuilder a (i, agendaId)

instance HasAbilities Agenda where
  getAbilities (Agenda a) = getAbilities a

instance RunMessage Agenda where
  runMessage msg (Agenda a) = Agenda <$> runMessage msg a

instance HasModifiersFor Agenda where
  getModifiersFor source target (Agenda a) = getModifiersFor source target a

instance Entity Agenda where
  type EntityId Agenda = AgendaId
  type EntityAttrs Agenda = AgendaAttrs
  toId = toId . toAttrs
  toAttrs (Agenda a) = toAttrs a
  overAttrs f (Agenda a) = Agenda $ overAttrs f a

instance TargetEntity Agenda where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Agenda where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance FromJSON Agenda where
  parseJSON v = flip (withObject "Agenda") v $ \o -> do
    cCode :: CardCode <- o .: "id"
    withAgendaCardCode cCode $ \(_ :: AgendaCard a) -> Agenda <$> parseJSON @a v

withAgendaCardCode
  :: CardCode
  -> (forall a. IsAgenda a => AgendaCard a -> r)
  -> r
withAgendaCardCode cCode f =
  case lookup cCode allAgendas of
    Nothing -> error $ "Unknown agenda: " <> show cCode
    Just (SomeAgendaCard a) -> f a

data SomeAgendaCard = forall a. IsAgenda a => SomeAgendaCard (AgendaCard a)

liftSomeAgendaCard :: (forall a. AgendaCard a -> b) -> SomeAgendaCard -> b
liftSomeAgendaCard f (SomeAgendaCard a) = f a

someAgendaCardCode :: SomeAgendaCard -> CardCode
someAgendaCardCode = liftSomeAgendaCard cbCardCode

allAgendas :: HashMap CardCode SomeAgendaCard
allAgendas = mapFromList $ map
  (toFst someAgendaCardCode)
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
