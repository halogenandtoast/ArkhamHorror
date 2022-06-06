module Arkham.Event.Attrs where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Event.Cards
import Arkham.Classes.Entity
import Arkham.Json
import Arkham.Card
import Arkham.Id
import Arkham.Name
import Arkham.Projection
import Arkham.Source
import Arkham.Target
import Arkham.Trait

class IsEvent a

type EventCard a = CardBuilder (InvestigatorId, EventId) a

data instance Field EventAttrs :: Type -> Type where
  EventAttachedTarget :: Field EventAttrs (Maybe Target)
  EventTraits :: Field EventAttrs (HashSet Trait)
  EventAbilities :: Field EventAttrs [Ability]
  EventOwner :: Field EventAttrs InvestigatorId

data EventAttrs = EventAttrs
  { eventCardCode :: CardCode
  , eventOriginalCardCode :: CardCode
  , eventId :: EventId
  , eventAttachedTarget :: Maybe Target
  , eventOwner :: InvestigatorId
  , eventDoom :: Int
  , eventExhausted :: Bool
  }
  deriving stock (Show, Eq, Generic)

attachedTargetL :: Lens' EventAttrs (Maybe Target)
attachedTargetL =
  lens eventAttachedTarget $ \m x -> m {eventAttachedTarget = x}

exhaustedL :: Lens' EventAttrs Bool
exhaustedL =
  lens eventExhausted $ \m x -> m {eventExhausted = x}

originalCardCodeL :: Lens' EventAttrs CardCode
originalCardCodeL =
  lens eventOriginalCardCode $ \m x -> m {eventOriginalCardCode = x}

allEventCards :: HashMap CardCode CardDef
allEventCards = allPlayerEventCards

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

event ::
  (EventAttrs -> a) -> CardDef -> CardBuilder (InvestigatorId, EventId) a
event f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \(iid, eid) ->
        f $
          EventAttrs
            { eventCardCode = toCardCode cardDef
            , eventOriginalCardCode = toCardCode cardDef
            , eventId = eid
            , eventAttachedTarget = Nothing
            , eventOwner = iid
            , eventDoom = 0
            , eventExhausted = False
            }
    }

instance Entity EventAttrs where
  type EntityId EventAttrs = EventId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventId
  toAttrs = id

instance Named EventAttrs where
  toName = toName . toCardDef

instance TargetEntity EventAttrs where
  toTarget = EventTarget . toId
  isTarget EventAttrs {eventId} (EventTarget eid) = eventId == eid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EventAttrs where
  toSource = EventSource . toId
  isSource EventAttrs {eventId} (EventSource eid) = eventId == eid
  isSource _ _ = False
