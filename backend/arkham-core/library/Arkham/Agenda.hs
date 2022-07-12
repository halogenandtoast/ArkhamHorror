module Arkham.Agenda (
  module Arkham.Agenda,
) where

import Arkham.Prelude

import Arkham.Agenda.Agendas
import Arkham.Agenda.Runner hiding (Agenda)
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
lookupAgenda agendaId =
  fromJustNote ("Unknown agenda: " <> show agendaId) $
    lookup agendaId allAgendas

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
    case cCode of
      -- Night of the Zealot
      -- The Gathering
      "01105" -> Agenda . WhatsGoingOn <$> parseJSON v
      "01106" -> Agenda . RiseOfTheGhouls <$> parseJSON v
      "01107" -> Agenda . TheyreGettingOut <$> parseJSON v
      -- The Midnight Masks
      "01121" -> Agenda . PredatorOrPrey <$> parseJSON v
      "01122" -> Agenda . TimeIsRunningShort <$> parseJSON v
      -- The Devourer Below
      "01143" -> Agenda . TheArkhamWoods <$> parseJSON v
      "01144" -> Agenda . TheRitualBegins <$> parseJSON v
      "01145" -> Agenda . VengeanceAwaits <$> parseJSON v
      -- The Dunwich Legacy
      -- Extracurricular Activity
      "02042" -> Agenda . QuietHalls <$> parseJSON v
      "02043" -> Agenda . DeadOfNight <$> parseJSON v
      "02044" -> Agenda . TheBeastUnleashed <$> parseJSON v
      -- The House Always Wins
      "02063" -> Agenda . TheCloverClub <$> parseJSON v
      "02064" -> Agenda . UndergroundMuscle <$> parseJSON v
      "02065" -> Agenda . ChaosInTheCloverClub <$> parseJSON v
      -- The Miskatonic Museum
      "02119" -> Agenda . RestrictedAccess <$> parseJSON v
      "02120" -> Agenda . ShadowsDeepen <$> parseJSON v
      "02121" -> Agenda . InEveryShadow <$> parseJSON v
      -- The Essex County Express
      "02160" -> Agenda . ATearInReality <$> parseJSON v
      "02161" -> Agenda . TheMawWidens <$> parseJSON v
      "02162" -> Agenda . RollingBackwards <$> parseJSON v
      "02163" -> Agenda . DrawnIn <$> parseJSON v
      "02164" -> Agenda . OutOfTime <$> parseJSON v
      -- Blood on the Altar
      "02196" -> Agenda . StrangeDisappearances <$> parseJSON v
      "02197" -> Agenda . TheOldOnesHunger <$> parseJSON v
      "02198" -> Agenda . FeedTheBeast <$> parseJSON v
      -- Undimensioned and Unseen
      "02237" -> Agenda . RampagingCreatures <$> parseJSON v
      "02238" -> Agenda . BidingItsTime <$> parseJSON v
      "02239" -> Agenda . HorrorsUnleashed <$> parseJSON v
      -- Where Doom Awaits
      "02275" -> Agenda . CallingForthTheOldOnes <$> parseJSON v
      "02276" -> Agenda . BeckoningForPower <$> parseJSON v
      -- Lost in Time and Space
      "02312" -> Agenda . AllIsOne <$> parseJSON v
      "02313" -> Agenda . PastPresentAndFuture <$> parseJSON v
      "02314" -> Agenda . BreakingThrough <$> parseJSON v
      "02315" -> Agenda . TheEndOfAllThings <$> parseJSON v
      -- The Path to Carcosa
      -- Curtain Call
      "03044" -> Agenda . TheThirdAct <$> parseJSON v
      "03045" -> Agenda . Encore <$> parseJSON v
      -- The Last King
      "03062" -> Agenda . FashionablyLate <$> parseJSON v
      "03063" -> Agenda . TheTerrifyingTruth <$> parseJSON v
      -- Echoes of the Past
      "03121" -> Agenda . TheTruthIsHidden <$> parseJSON v
      "03122" -> Agenda . RansackingTheManor <$> parseJSON v
      "03123" -> Agenda . SecretsBetterLeftHidden <$> parseJSON v
      -- The Unspeakable Oath
      "03160" -> Agenda . LockedInside <$> parseJSON v
      "03161" -> Agenda . TorturousDescent <$> parseJSON v
      "03162" -> Agenda . HisDomain <$> parseJSON v
      -- A Phantom of Truth
      "03201" -> Agenda . TheFirstNight <$> parseJSON v
      "03202" -> Agenda . TheSecondNight <$> parseJSON v
      "03203" -> Agenda . TheThirdNight <$> parseJSON v
      -- The Pallid Mask
      "03241" -> Agenda . EmpireOfTheDead <$> parseJSON v
      "03242" -> Agenda . EmpireOfTheUndead <$> parseJSON v
      -- Black Stars Rise
      "03275" -> Agenda . TheTideRises <$> parseJSON v
      "03276a" -> Agenda . LetTheStormRageTheFloodBelow <$> parseJSON v
      "03276b" -> Agenda . LetTheStormRageTheVortexAbove <$> parseJSON v
      "03277" -> Agenda . TheCityFloods <$> parseJSON v
      "03278" -> Agenda . TheRitualBeginsBlackStarsRise <$> parseJSON v
      "03279a" -> Agenda . TheEntityAboveTheFloodBelow <$> parseJSON v
      "03279b" -> Agenda . TheEntityAboveTheVortexAbove <$> parseJSON v
      "03280" -> Agenda . SwallowedSky <$> parseJSON v
      -- Dim Carcosa
      "03317" -> Agenda . MadnessCoils <$> parseJSON v
      "03318" -> Agenda . MadnessDrowns <$> parseJSON v
      "03319" -> Agenda . MadnessDies <$> parseJSON v
      -- Return to the Night of the Zealot
      -- Return to the Midnight Masks
      "50026" -> Agenda . ReturnToPredatorOrPrey <$> parseJSON v
      -- Curse of the Rougarou
      "81002" -> Agenda . ACreatureOfTheBayou <$> parseJSON v
      "81003" -> Agenda . TheRougarouFeeds <$> parseJSON v
      "81004" -> Agenda . TheCurseSpreads <$> parseJSON v
      -- Carnevale of Horrors
      "82002" -> Agenda . TheFestivitiesBegin <$> parseJSON v
      "82003" -> Agenda . TheShadowOfTheEclipse <$> parseJSON v
      "82004" -> Agenda . ChaosAtTheCarnevale <$> parseJSON v
      _ -> error "invalid agenda"

allAgendas :: HashMap AgendaId (Int -> Agenda)
allAgendas = mapFromList $ map
  (\cb -> (AgendaId (cbCardCode cb), \deckId -> cbCardBuilder cb (deckId, AgendaId (cbCardCode cb))))
  [ -- Night of the Zealot
  -- The Gathering
    Agenda <$> whatsGoingOn
  , Agenda <$> riseOfTheGhouls
  , Agenda <$> theyreGettingOut
  -- The Midnight Masks
  , Agenda <$> predatorOrPrey
  , Agenda <$> timeIsRunningShort
  -- The Devourer Below
  , Agenda <$> theArkhamWoods
  , Agenda <$> theRitualBegins
  , Agenda <$> vengeanceAwaits
  -- The Dunwich Legacy
  -- Extracurricular Activity
  , Agenda <$> quietHalls
  , Agenda <$> deadOfNight
  , Agenda <$> theBeastUnleashed
  -- The House Always Wins
  , Agenda <$> theCloverClub
  , Agenda <$> undergroundMuscle
  , Agenda <$> chaosInTheCloverClub
  -- The Miskatonic Museum
  , Agenda <$> restrictedAccess
  , Agenda <$> shadowsDeepen
  , Agenda <$> inEveryShadow
  -- The Essex County Express
  , Agenda <$> aTearInReality
  , Agenda <$> theMawWidens
  , Agenda <$> rollingBackwards
  , Agenda <$> drawnIn
  , Agenda <$> outOfTime
  -- Blood on the Altar
  , Agenda <$> strangeDisappearances
  , Agenda <$> theOldOnesHunger
  , Agenda <$> feedTheBeast
  -- Undimensioned and Unseen
  , Agenda <$> rampagingCreatures
  , Agenda <$> bidingItsTime
  , Agenda <$> horrorsUnleashed
  -- Where Doom Awaits
  , Agenda <$> callingForthTheOldOnes
  , Agenda <$> beckoningForPower
  -- Lost in Time and Space
  , Agenda <$> allIsOne
  , Agenda <$> pastPresentAndFuture
  , Agenda <$> breakingThrough
  , Agenda <$> theEndOfAllThings
  -- The Path to Carcosa
  -- Curtain Call
  , Agenda <$> theThirdAct
  , Agenda <$> encore
  -- The Last King
  , Agenda <$> fashionablyLate
  , Agenda <$> theTerrifyingTruth
  -- Echoes of the Past
  , Agenda <$> theTruthIsHidden
  , Agenda <$> ransackingTheManor
  , Agenda <$> secretsBetterLeftHidden
  -- The Unspeakable Oath
  , Agenda <$> lockedInside
  , Agenda <$> torturousDescent
  , Agenda <$> hisDomain
  -- A Phantom of Truth
  , Agenda <$> theFirstNight
  , Agenda <$> theSecondNight
  , Agenda <$> theThirdNight
  -- The Pallid Mask
  , Agenda <$> empireOfTheDead
  , Agenda <$> empireOfTheUndead
  -- Black Stars Rise
  , Agenda <$> theTideRises
  , Agenda <$> letTheStormRageTheFloodBelow
  , Agenda <$> letTheStormRageTheVortexAbove
  , Agenda <$> theCityFloods
  , Agenda <$> theRitualBeginsBlackStarsRise
  , Agenda <$> theEntityAboveTheFloodBelow
  , Agenda <$> theEntityAboveTheVortexAbove
  , Agenda <$> swallowedSky
  -- Dim Carcosa
  , Agenda <$> madnessCoils
  , Agenda <$> madnessDrowns
  , Agenda <$> madnessDies
  -- Return to the Night of the Zealot
  -- Return to the Midnight Masks
  , Agenda <$> returnToPredatorOrPrey
  -- Curse of the Rougarou
  , Agenda <$> aCreatureOfTheBayou
  , Agenda <$> theRougarouFeeds
  , Agenda <$> theCurseSpreads
  -- Carnevale of Horrors
  , Agenda <$> theFestivitiesBegin
  , Agenda <$> theShadowOfTheEclipse
  , Agenda <$> chaosAtTheCarnevale
  ]
