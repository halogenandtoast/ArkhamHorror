module Arkham.Types.Event.Attrs where

import Arkham.Prelude

import Arkham.Event.Cards
import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Source
import Arkham.Types.Target

class IsEvent a

type EventCard a = CardBuilder (InvestigatorId, EventId) a

data EventAttrs = EventAttrs
  { eventCardCode :: CardCode
  , eventOriginalCardCode :: CardCode
  , eventId :: EventId
  , eventAttachedTarget :: Maybe Target
  , eventOwner :: InvestigatorId
  , eventDoom :: Int
  }
  deriving stock (Show, Eq, Generic)

attachedTargetL :: Lens' EventAttrs (Maybe Target)
attachedTargetL =
  lens eventAttachedTarget $ \m x -> m { eventAttachedTarget = x }

originalCardCodeL :: Lens' EventAttrs CardCode
originalCardCodeL =
  lens eventOriginalCardCode $ \m x -> m { eventOriginalCardCode = x }

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

unshiftEffect
  :: (HasQueue env, MonadReader env m, MonadIO m)
  => EventAttrs
  -> Target
  -> m ()
unshiftEffect attrs target = pushAll
  [ CreateEffect (cdCardCode $ toCardDef attrs) Nothing (toSource attrs) target
  , Discard $ toTarget attrs
  ]

event
  :: (EventAttrs -> a) -> CardDef -> CardBuilder (InvestigatorId, EventId) a
event f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(iid, eid) -> f $ EventAttrs
    { eventCardCode = toCardCode cardDef
    , eventOriginalCardCode = toCardCode cardDef
    , eventId = eid
    , eventAttachedTarget = Nothing
    , eventOwner = iid
    , eventDoom = 0
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
  isTarget EventAttrs { eventId } (EventTarget eid) = eventId == eid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EventAttrs where
  toSource = EventSource . toId
  isSource EventAttrs { eventId } (EventSource eid) = eventId == eid
  isSource _ _ = False

instance HasQueue env => RunMessage env EventAttrs where
  runMessage msg a@EventAttrs {..} = case msg of
    SetOriginalCardCode cardCode -> pure $ a & originalCardCodeL .~ cardCode
    InvestigatorEliminated iid
      | eventAttachedTarget == Just (InvestigatorTarget iid) -> a
      <$ push (Discard (EventTarget eventId))
    AttachEvent eid target | eid == eventId ->
      pure $ a & attachedTargetL ?~ target
    _ -> pure a
