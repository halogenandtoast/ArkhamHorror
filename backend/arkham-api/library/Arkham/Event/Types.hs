{-# LANGUAGE TemplateHaskell #-}

module Arkham.Event.Types where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Calculation
import Arkham.Card
import Arkham.ChaosToken.Types (ChaosToken)
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Customization
import Arkham.Event.Cards
import Arkham.Id
import Arkham.Json
import Arkham.Matcher.Base (Be (..))
import Arkham.Matcher.Event
import Arkham.Message
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Source
import Arkham.Taboo.Types
import Arkham.Target
import Arkham.Trait
import Arkham.Window (Window)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.TH
import Data.Data
import Data.Map.Strict qualified as Map
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
  EventController :: Field Event InvestigatorId
  EventDoom :: Field Event Int
  EventCard :: Field Event Card
  EventCardId :: Field Event CardId
  EventSealedChaosTokens :: Field Event [ChaosToken]
  EventUses :: Field Event (Map UseType Int)
  EventTokens :: Field Event (Map UseType Int)
  EventWindows :: Field Event [Window]
  EventCustomizations :: Field Event Customizations
  EventMeta :: Field Event Value

data instance Field (InHandEntity Event) :: Type -> Type where
  InHandEventCardId :: Field (InHandEntity Event) CardId

eventAttachedTarget :: EventAttrs -> Maybe Target
eventAttachedTarget = placementToAttached . eventPlacement

data EventAttrs = EventAttrs
  { eventCardCode :: CardCode
  , eventCardId :: CardId
  , eventOriginalCardCode :: CardCode
  , eventId :: EventId
  , eventOwner :: InvestigatorId
  , eventController :: InvestigatorId
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
  , eventTokens :: Map UseType Int
  , eventCustomizations :: Customizations
  , eventPrintedUses :: Uses GameCalculation
  , eventTaboo :: Maybe TabooList
  , eventMutated :: Maybe Text -- for art display
  }
  deriving stock (Show, Eq)

allEventCards :: Map CardCode CardDef
allEventCards = allPlayerEventCards

instance Be EventAttrs EventMatcher where
  be = EventWithId . eventId

instance AsId EventAttrs where
  type IdOf EventAttrs = EventId
  asId = toId

instance Is EventAttrs EventId where
  is = (==) . toId

instance HasField "uses" EventAttrs (Map UseType Int) where
  getField = Map.filterWithKey (\k _ -> tokenIsUse k) . coerce . eventTokens

instance HasField "use" EventAttrs (UseType -> Int) where
  getField a uType = findWithDefault 0 uType a.uses

instance HasField "ready" EventAttrs Bool where
  getField = not . eventExhausted

instance HasField "taboo" EventAttrs (Maybe TabooList) where
  getField = eventTaboo

instance HasField "customizations" EventAttrs Customizations where
  getField = eventCustomizations

instance HasField "windows" EventAttrs [Window] where
  getField = eventWindows

instance HasField "attachedTo" EventAttrs (Maybe Target) where
  getField = eventAttachedTarget

instance HasField "id" EventAttrs EventId where
  getField = eventId

instance HasField "meta" EventAttrs Value where
  getField = eventMeta

instance HasField "tokens" EventAttrs Tokens where
  getField = eventTokens

instance HasField "placement" EventAttrs Placement where
  getField = eventPlacement

instance HasField "payment" EventAttrs Payment where
  getField = eventPayment

instance HasField "owner" EventAttrs InvestigatorId where
  getField = eventOwner

instance HasField "controller" EventAttrs InvestigatorId where
  getField = eventController

instance HasField "ability" EventAttrs (Int -> Source) where
  getField this = toAbilitySource this

instance HasField "doom" EventAttrs Int where
  getField = countTokens Doom . eventTokens

instance HasField "cardId" EventAttrs CardId where
  getField = toCardId
  {-# INLINE getField #-}

instance HasCardCode EventAttrs where
  toCardCode = eventCardCode

instance HasCardDef EventAttrs where
  toCardDef a = case lookup (eventCardCode a) allEventCards of
    Just def -> def
    Nothing -> error $ "missing card def for asset " <> show (eventCardCode a)

instance IsCard EventAttrs where
  toCard = defaultToCard
  toCardId = eventCardId
  toCardOwner = Just . eventOwner
  toCustomizations = eventCustomizations

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
            , eventController = iid
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
            , eventTokens = mempty
            , eventCustomizations = mempty
            , eventPrintedUses = cdUses cardDef
            , eventTaboo = Nothing
            , eventMutated = Nothing
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
  isTarget attrs@EventAttrs {..} = \case
    EventTarget eid -> eventId == eid
    CardCodeTarget cardCode -> cdCardCode (toCardDef attrs) == cardCode
    CardIdTarget cardId -> cardId == eventCardId
    SkillTestInitiatorTarget target -> isTarget attrs target
    _ -> False

instance Sourceable EventAttrs where
  toSource = EventSource . toId
  isSource EventAttrs {eventId} (EventSource eid) = eventId == eid
  isSource _ _ = False

data Event = forall a. IsEvent a => Event a

instance Data Event where
  gunfold _ _ _ = error "gunfold(Event)"
  toConstr _ = error "toConstr(Event)"
  dataTypeOf _ = error "dataTypeOf(Event)"

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
  getModifiersFor (Event a) = getModifiersFor a

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
  toCard e = case lookupCard (eventOriginalCardCode . toAttrs $ e) (toCardId e) of
    PlayerCard pc ->
      PlayerCard
        $ pc
          { pcCustomizations = toCustomizations e
          , pcOwner = toCardOwner e
          , pcTabooList = attr eventTaboo e
          , pcMutated = attr eventMutated e
          }
    ec -> ec
  toCardOwner = toCardOwner . toAttrs
  toCustomizations = toCustomizations . toAttrs

getEventId :: Event -> EventId
getEventId = eventId . toAttrs

ownerOfEvent :: Event -> InvestigatorId
ownerOfEvent = eventOwner . toAttrs

instance HasField "owner" Event InvestigatorId where
  getField = attr eventOwner

data SomeEventCard = forall a. IsEvent a => SomeEventCard (EventCard a)

liftSomeEventCard :: (forall a. EventCard a -> b) -> SomeEventCard -> b
liftSomeEventCard f (SomeEventCard a) = f a

someEventCardCode :: SomeEventCard -> CardCode
someEventCardCode = liftSomeEventCard cbCardCode

makeLensesWith suffixedFields ''EventAttrs

setMeta :: ToJSON a => a -> EventAttrs -> EventAttrs
setMeta a = metaL .~ toJSON a

overMeta :: (FromJSON a, ToJSON a) => (a -> a) -> EventAttrs -> EventAttrs
overMeta f = metaL %~ (\m -> toJSON (f $ toResult m))

getEventMeta :: FromJSON a => EventAttrs -> Maybe a
getEventMeta a = case fromJSON (eventMeta a) of
  Error _ -> Nothing
  Success a' -> Just a'

setMetaKey :: (ToJSON a, HasCallStack) => Key -> a -> EventAttrs -> EventAttrs
setMetaKey k v attrs = case attrs.meta of
  Object o -> attrs {eventMeta = Object $ KeyMap.insert k (toJSON v) o}
  Null -> attrs {eventMeta = object [k .= v]}
  _ -> error $ "Could not insert meta key, meta is not Null or Object: " <> show attrs.meta

getMetaKey :: Key -> EventAttrs -> Bool
getMetaKey k attrs = case attrs.meta of
  Object o -> case KeyMap.lookup k o of
    Nothing -> False
    Just v -> case fromJSON v of
      Error _ -> False
      Success v' -> v'
  _ -> False

getMetaKeyDefault :: FromJSON a => Key -> a -> EventAttrs -> a
getMetaKeyDefault k def attrs = case attrs.meta of
  Object o -> case KeyMap.lookup k o of
    Nothing -> def
    Just v -> case fromJSON v of
      Error _ -> def
      Success v' -> v'
  _ -> def

$(deriveJSON (aesonOptions $ Just "event") ''EventAttrs)
