module Arkham.Event.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards
import Arkham.Classes.Entity
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.HasAbilities
import Arkham.Classes.RunMessage.Internal
import Arkham.Json
import Arkham.Card
import Arkham.Id
import Arkham.Name
import Arkham.Message
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Token ( Token )
import Arkham.Trait
import Data.HashMap.Strict qualified as HashMap
import Data.Typeable

class (Typeable a, ToJSON a, FromJSON a, Eq a, Show a, HasAbilities a, HasModifiersFor a, RunMessage a, Entity a, EntityId a ~ EventId, EntityAttrs a ~ EventAttrs) => IsEvent a

type EventCard a = CardBuilder (InvestigatorId, EventId) a

data instance Field Event :: Type -> Type where
  EventPlacement :: Field Event Placement
  EventTraits :: Field Event (HashSet Trait)
  EventAbilities :: Field Event [Ability]
  EventOwner :: Field Event InvestigatorId
  EventDoom :: Field Event Int
  EventCard :: Field Event Card
  EventSealedTokens :: Field Event [Token]

-- These could be different, update in the future
eventController :: EventAttrs -> InvestigatorId
eventController = eventOwner

eventAttachedTarget :: EventAttrs -> Maybe Target
eventAttachedTarget = placementToAttached . eventPlacement

data EventAttrs = EventAttrs
  { eventCardCode :: CardCode
  , eventOriginalCardCode :: CardCode
  , eventId :: EventId
  , eventOwner :: InvestigatorId
  , eventDoom :: Int
  , eventExhausted :: Bool
  , eventBeingPaidFor :: Bool
  , eventPaymentMessages :: [Message]
  , eventSealedTokens :: [Token]
  , eventPlacement :: Placement
  , eventAfterPlay :: AfterPlayStrategy
  }
  deriving stock (Show, Eq, Generic)

afterPlayL :: Lens' EventAttrs AfterPlayStrategy
afterPlayL = lens eventAfterPlay $ \m x -> m { eventAfterPlay = x }

placementL :: Lens' EventAttrs Placement
placementL = lens eventPlacement $ \m x -> m { eventPlacement = x }

exhaustedL :: Lens' EventAttrs Bool
exhaustedL =
  lens eventExhausted $ \m x -> m {eventExhausted = x}

paymentMessagesL :: Lens' EventAttrs [Message]
paymentMessagesL =
  lens eventPaymentMessages $ \m x -> m {eventPaymentMessages = x}

beingPaidForL :: Lens' EventAttrs Bool
beingPaidForL =
  lens eventBeingPaidFor $ \m x -> m {eventBeingPaidFor = x}

originalCardCodeL :: Lens' EventAttrs CardCode
originalCardCodeL =
  lens eventOriginalCardCode $ \m x -> m {eventOriginalCardCode = x}

sealedTokensL :: Lens' EventAttrs [Token]
sealedTokensL = lens eventSealedTokens $ \m x -> m { eventSealedTokens = x }

allEventCards :: HashMap CardCode SomeCardDef
allEventCards = HashMap.map (SomeCardDef SEventType) allPlayerEventCards

instance HasCardCode EventAttrs where
  toCardCode = eventCardCode

instance HasCardDef EventAttrs where
  toCardDef a = case lookup (eventCardCode a) allEventCards of
    Just def -> def
    Nothing -> error $ "missing card def for asset " <> show (eventCardCode a)

instance ToJSON EventAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "event"
  toEncoding = genericToEncoding $ aesonOptions $ Just "event"

instance FromJSON EventAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "event"

instance IsCard EventAttrs where
  toCardId = unEventId . eventId
  toCardOwner = Just . eventOwner

eventWith :: (EventAttrs -> a) -> CardDef 'EventType -> (EventAttrs -> EventAttrs) -> CardBuilder (InvestigatorId, EventId) a
eventWith f cardDef g = Arkham.Event.Types.event (f . g) cardDef

event ::
  (EventAttrs -> a) -> CardDef 'EventType -> CardBuilder (InvestigatorId, EventId) a
event f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(iid, eid) ->
        f $
          EventAttrs
            { eventCardCode = toCardCode cardDef
            , eventOriginalCardCode = toCardCode cardDef
            , eventId = eid
            , eventOwner = iid
            , eventDoom = 0
            , eventExhausted = False
            -- currently only relevant to time warp
            , eventBeingPaidFor = False
            , eventPaymentMessages = []
            , eventSealedTokens = []
            , eventPlacement = Unplaced
            , eventAfterPlay = DiscardThis
            }
    }

instance Entity EventAttrs where
  type EntityId EventAttrs = EventId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventId
  toAttrs = id
  overAttrs f = f

instance Named EventAttrs where
  toName = withCardDef toName

instance Targetable EventAttrs where
  toTarget = EventTarget . toId
  isTarget EventAttrs {eventId} (EventTarget eid) = eventId == eid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EventAttrs where
  toSource = EventSource . toId
  isSource EventAttrs {eventId} (EventSource eid) = eventId == eid
  isSource _ _ = False

data Event = forall a. IsEvent a => Event a

instance Eq Event where
  (Event (a :: a)) == (Event (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Event where
  show (Event a) = show a

instance ToJSON Event where
  toJSON (Event a) = toJSON a

instance HasCardCode Event where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Event where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Event where
  getAbilities (Event a) = getAbilities a

instance HasModifiersFor Event where
  getModifiersFor target (Event a) = getModifiersFor target a

instance Entity Event where
  type EntityId Event = EventId
  type EntityAttrs Event = EventAttrs
  toId = toId . toAttrs
  toAttrs (Event a) = toAttrs a
  overAttrs f (Event a) = Event $ overAttrs f a

instance Targetable Event where
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

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs

data SomeEventCard = forall a. IsEvent a => SomeEventCard (EventCard a)

liftSomeEventCard :: (forall a. EventCard a -> b) -> SomeEventCard -> b
liftSomeEventCard f (SomeEventCard a) = f a

someEventCardCode :: SomeEventCard -> CardCode
someEventCardCode = liftSomeEventCard cbCardCode

