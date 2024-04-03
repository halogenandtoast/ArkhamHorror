{-# LANGUAGE TemplateHaskell #-}

module Arkham.Event.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken (ChaosToken)
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Event.Cards
import Arkham.Id
import Arkham.Json
import Arkham.Message
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait
import Arkham.Window (Window)
import Data.Typeable
import GHC.Records

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ EventId
  , EntityAttrs a ~ EventAttrs
  ) =>
  IsEvent a

type EventCard a = CardBuilder (InvestigatorId, EventId) a

data instance Field Event :: Type -> Type where
  EventPlacement :: Field Event Placement
  EventTraits :: Field Event (Set Trait)
  EventAbilities :: Field Event [Ability]
  EventOwner :: Field Event InvestigatorId
  EventDoom :: Field Event Int
  EventCard :: Field Event Card
  EventCardId :: Field Event CardId
  EventSealedChaosTokens :: Field Event [ChaosToken]

data instance Field (InHandEntity Event) :: Type -> Type where
  InHandEventCardId :: Field (InHandEntity Event) CardId

-- These could be different, update in the future
eventController :: EventAttrs -> InvestigatorId
eventController = eventOwner

eventAttachedTarget :: EventAttrs -> Maybe Target
eventAttachedTarget = placementToAttached . eventPlacement

data EventAttrs = EventAttrs
  { eventCardCode :: CardCode
  , eventCardId :: CardId
  , eventOriginalCardCode :: CardCode
  , eventId :: EventId
  , eventOwner :: InvestigatorId
  , eventDoom :: Int
  , eventExhausted :: Bool
  , eventBeingPaidFor :: Bool
  , eventPayment :: Payment
  , eventPaymentMessages :: [Message]
  , eventSealedChaosTokens :: [ChaosToken]
  , eventCardsUnderneath :: [Card]
  , eventPlacement :: Placement
  , eventAfterPlay :: AfterPlayStrategy
  , eventPlayedFrom :: Zone
  , eventWindows :: [Window]
  , eventTarget :: Maybe Target
  , eventMeta :: Value
  }
  deriving stock (Show, Eq, Generic)

allEventCards :: Map CardCode CardDef
allEventCards = allPlayerEventCards

instance Is EventAttrs EventId where
  is = (==) . toId

instance HasField "id" EventAttrs EventId where
  getField = eventId

instance HasField "meta" EventAttrs Value where
  getField = eventMeta

instance HasField "placement" EventAttrs Placement where
  getField = eventPlacement

instance HasField "payment" EventAttrs Payment where
  getField = eventPayment

instance HasField "owner" EventAttrs InvestigatorId where
  getField = eventOwner

instance HasField "controller" EventAttrs InvestigatorId where
  getField = eventOwner

instance HasField "ability" EventAttrs (Int -> Source) where
  getField this = toAbilitySource this

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
  parseJSON = withObject "EventAttrs" \o -> do
    eventCardCode <- o .: "cardCode"
    eventCardId <- o .: "cardId"
    eventOriginalCardCode <- o .: "originalCardCode"
    eventId <- o .: "id"
    eventOwner <- o .: "owner"
    eventDoom <- o .: "doom"
    eventExhausted <- o .: "exhausted"
    eventBeingPaidFor <- o .: "beingPaidFor"
    eventPayment <- o .:? "payment" .!= NoPayment
    eventPaymentMessages <- o .: "paymentMessages"
    eventSealedChaosTokens <- o .: "sealedChaosTokens"
    eventCardsUnderneath <- o .: "cardsUnderneath"
    eventPlacement <- o .: "placement"
    eventAfterPlay <- o .: "afterPlay"
    eventPlayedFrom <- o .: "playedFrom"
    eventWindows <- o .: "windows"
    eventTarget <- o .: "target"
    eventMeta <- o .:? "meta" .!= Null

    pure EventAttrs {..}

instance IsCard EventAttrs where
  toCard = defaultToCard
  toCardId = eventCardId
  toCardOwner = Just . eventOwner

eventWith
  :: (EventAttrs -> a)
  -> CardDef
  -> (EventAttrs -> EventAttrs)
  -> CardBuilder (InvestigatorId, EventId) a
eventWith f cardDef g = Arkham.Event.Types.event (f . g) cardDef

event
  :: (EventAttrs -> a) -> CardDef -> CardBuilder (InvestigatorId, EventId) a
event f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (iid, eid) ->
        f
          $ EventAttrs
            { eventCardCode = toCardCode cardDef
            , eventCardId = cardId
            , eventOriginalCardCode = toCardCode cardDef
            , eventId = eid
            , eventOwner = iid
            , eventDoom = 0
            , eventExhausted = False
            , -- currently only relevant to time warp
              eventBeingPaidFor = False
            , eventPayment = NoPayment
            , eventPaymentMessages = []
            , eventSealedChaosTokens = []
            , eventCardsUnderneath = []
            , eventPlacement = Unplaced
            , eventAfterPlay = DiscardThis
            , eventPlayedFrom = FromHand -- defaults but will be overwritten when needed
            , eventWindows = []
            , eventTarget = Nothing
            , eventMeta = Null
            }
    }

instance Entity EventAttrs where
  type EntityId EventAttrs = EventId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventId
  toAttrs = id
  overAttrs f = f

instance Named EventAttrs where
  toName = toName . toCardDef

instance Targetable EventAttrs where
  toTarget = EventTarget . toId
  isTarget EventAttrs {eventId} (EventTarget eid) = eventId == eid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance Sourceable EventAttrs where
  toSource = EventSource . toId
  isSource EventAttrs {eventId} (EventSource eid) = eventId == eid
  isSource _ _ = False

data Event = forall a. IsEvent a => Event a

instance HasField "placement" Event Placement where
  getField (Event a) = (toAttrs a).placement

instance Named Event where
  toName (Event e) = toName (toAttrs e)
  {-# INLINE toName #-}

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

instance Sourceable Event where
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

makeLensesWith suffixedFields ''EventAttrs

setMeta :: ToJSON a => a -> EventAttrs -> EventAttrs
setMeta a = metaL .~ toJSON a
