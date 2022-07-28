module Arkham.Event where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Events
import Arkham.Event.Runner
import Arkham.Id
import Data.Typeable

data Event = forall a. IsEvent a => Event a

instance Eq Event where
  (Event (a :: a)) == (Event (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Event where
  show (Event a) = show a

instance ToJSON Event where
  toJSON (Event a) = toJSON a

createEvent :: IsCard a => a -> InvestigatorId -> Event
createEvent a iid = lookupEvent (toCardCode a) iid (EventId $ toCardId a)

instance HasCardCode Event where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Event where
  getAbilities (Event a) = getAbilities a

instance HasModifiersFor Event where
  getModifiersFor source target (Event a) = getModifiersFor source target a

instance RunMessage Event where
  runMessage msg (Event a) = Event <$> runMessage msg a

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs (Event a) = toAttrs a
  overAttrs f (Event a) = Event $ overAttrs f a

instance TargetEntity Event where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Event where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Event where
  toCardId = toCardId . toAttrs
  toCard e = lookupCard (eventOriginalCardCode . toAttrs $ e) (toCardId e)
  toCardOwner = toCardOwner . toAttrs

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

lookupEvent :: CardCode -> (InvestigatorId -> EventId -> Event)
lookupEvent cardCode = case lookup cardCode allEvents of
  Nothing -> error $ "Unknown event: " <> show cardCode
  Just (SomeEventCard a) -> \i e -> Event $ cbCardBuilder a (i, e)

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs

instance FromJSON Event where
  parseJSON v = flip (withObject "Event") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withEventCardCode cCode $ \(_ :: EventCard a) -> Event <$> parseJSON @a v

withEventCardCode
  :: CardCode
  -> (forall a. IsEvent a => EventCard a -> r)
  -> r
withEventCardCode cCode f =
  case lookup cCode allEvents of
    Nothing -> error $ "Unknown event: " <> show cCode
    Just (SomeEventCard a) -> f a

data SomeEventCard = forall a. IsEvent a => SomeEventCard (EventCard a)

liftSomeEventCard :: (forall a. EventCard a -> b) -> SomeEventCard -> b
liftSomeEventCard f (SomeEventCard a) = f a

someEventCardCode :: SomeEventCard -> CardCode
someEventCardCode = liftSomeEventCard cbCardCode

allEvents :: HashMap CardCode SomeEventCard
allEvents = mapFromList $ map
  (toFst someEventCardCode)
  [ SomeEventCard onTheLam
  , SomeEventCard darkMemory
  , SomeEventCard evidence
  , SomeEventCard dodge
  , SomeEventCard dynamiteBlast
  , SomeEventCard extraAmmunition1
  , SomeEventCard mindOverMatter
  , SomeEventCard workingAHunch
  , SomeEventCard barricade
  , SomeEventCard crypticResearch4
  , SomeEventCard elusive
  , SomeEventCard backstab
  , SomeEventCard sneakAttack
  , SomeEventCard sureGamble3
  , SomeEventCard hotStreak4
  , SomeEventCard drawnToTheFlame
  , SomeEventCard wardOfProtection
  , SomeEventCard blindingLight
  , SomeEventCard mindWipe1
  , SomeEventCard blindingLight2
  , SomeEventCard cunningDistraction
  , SomeEventCard lookWhatIFound
  , SomeEventCard lucky
  , SomeEventCard closeCall2
  , SomeEventCard lucky2
  , SomeEventCard willToSurvive3
  , SomeEventCard emergencyCache
  , SomeEventCard searchForTheTruth
  , SomeEventCard taunt
  , SomeEventCard teamwork
  , SomeEventCard taunt2
  , SomeEventCard shortcut
  , SomeEventCard seekingAnswers
  , SomeEventCard thinkOnYourFeet
  , SomeEventCard bindMonster2
  , SomeEventCard baitAndSwitch
  , SomeEventCard emergencyAid
  , SomeEventCard iveGotAPlan
  , SomeEventCard contraband
  , SomeEventCard delveTooDeep
  , SomeEventCard oops
  , SomeEventCard flare1
  , SomeEventCard standTogether3
  , SomeEventCard imOuttaHere
  , SomeEventCard hypnoticGaze
  , SomeEventCard lure1
  , SomeEventCard preparedForTheWorst
  , SomeEventCard preposterousSketches
  , SomeEventCard emergencyCache2
  , SomeEventCard ifItBleeds
  , SomeEventCard exposeWeakness1
  , SomeEventCard iveHadWorse4
  , SomeEventCard aceInTheHole3
  , SomeEventCard moonlightRitual
  , SomeEventCard aChanceEncounter
  , SomeEventCard momentOfRespite3
  , SomeEventCard monsterSlayer5
  , SomeEventCard decipheredReality5
  , SomeEventCard wardOfProtection5
  , SomeEventCard thePaintedWorld
  , SomeEventCard buryThemDeep
  , SomeEventCard improvisation
  , SomeEventCard letMeHandleThis
  , SomeEventCard everVigilant1
  , SomeEventCard noStoneUnturned
  , SomeEventCard sleightOfHand
  , SomeEventCard daringManeuver
  , SomeEventCard uncageTheSoul
  , SomeEventCard astralTravel
  , SomeEventCard hidingSpot
  , SomeEventCard heroicRescue
  , SomeEventCard anatomicalDiagrams
  , SomeEventCard ambush1
  , SomeEventCard forewarned1
  , SomeEventCard sneakAttack2
  , SomeEventCard stormOfSpirits
  , SomeEventCard fightOrFlight
  , SomeEventCard aTestOfWill1
  , SomeEventCard devilsLuck
  , SomeEventCard callingInFavors
  , SomeEventCard illSeeYouInHell
  , SomeEventCard logicalReasoning
  , SomeEventCard cheapShot
  , SomeEventCard quantumFlux
  , SomeEventCard recharge2
  , SomeEventCard snareTrap2
  , SomeEventCard manoAMano1
  , SomeEventCard shortcut2
  , SomeEventCard waylay
  , SomeEventCard aChanceEncounter2
  , SomeEventCard emergencyCache3
  , SomeEventCard onTheHunt
  , SomeEventCard guidance
  , SomeEventCard narrowEscape
  , SomeEventCard wardOfProtection2
  , SomeEventCard trueSurvivor3
  , SomeEventCard eatLead2
  , SomeEventCard eideticMemory3
  , SomeEventCard noStoneUnturned5
  , SomeEventCard cheatDeath5
  , SomeEventCard timeWarp2
  , SomeEventCard infighting3
  , SomeEventCard smuggledGoods
  , SomeEventCard trusted
  , SomeEventCard reliable1
  , SomeEventCard unearthTheAncients
  , SomeEventCard eavesdrop
  , SomeEventCard youHandleThisOne
  , SomeEventCard darkProphecy
  , SomeEventCard improvisedWeapon
  , SomeEventCard dumbLuck
  , SomeEventCard darkPact
  , SomeEventCard secondWind
  , SomeEventCard liveAndLearn
  , SomeEventCard wingingIt
  , SomeEventCard bloodRite
  , SomeEventCard astoundingRevelation
  , SomeEventCard firstWatch
  , SomeEventCard scroungeForSupplies
  , SomeEventCard dynamiteBlast2
  , SomeEventCard barricade3
  , SomeEventCard hotStreak2
  , SomeEventCard mindWipe3
  , SomeEventCard preposterousSketches2
  , SomeEventCard contraband2
  , SomeEventCard cleanThemOut
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
  , SomeEventCard iveGotAPlan2
  , SomeEventCard willToSurvive
  , SomeEventCard aTestOfWill
  , SomeEventCard gritYourTeeth
  , SomeEventCard aTestOfWill2
  , SomeEventCard lookWhatIFound2
  , SomeEventCard dumbLuck2
  , SomeEventCard lucky3
  ]
