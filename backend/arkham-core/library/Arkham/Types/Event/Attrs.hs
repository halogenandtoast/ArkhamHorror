{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Event.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Target

type EventCard a = (InvestigatorId -> EventId -> a)

data EventAttrs = EventAttrs
  { eventCardDef :: CardDef
  , eventId :: EventId
  , eventAttachedTarget :: Maybe Target
  , eventOwner :: InvestigatorId
  , eventDoom :: Int
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''EventAttrs

instance HasCardDef EventAttrs where
  toCardDef = eventCardDef

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
unshiftEffect attrs target = unshiftMessages
  [ CreateEffect (cdCardCode $ toCardDef attrs) Nothing (toSource attrs) target
  , Discard $ toTarget attrs
  ]

event :: (EventAttrs -> a) -> CardDef -> InvestigatorId -> EventId -> a
event f cardDef iid eid = f $ EventAttrs
  { eventCardDef = cardDef
  , eventId = eid
  , eventAttachedTarget = Nothing
  , eventOwner = iid
  , eventDoom = 0
  }

instance Entity EventAttrs where
  type EntityId EventAttrs = EventId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventId
  toAttrs = id

instance NamedEntity EventAttrs where
  toName = cdName . toCardDef

instance TargetEntity EventAttrs where
  toTarget = EventTarget . toId
  isTarget EventAttrs { eventId } (EventTarget eid) = eventId == eid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance SourceEntity EventAttrs where
  toSource = EventSource . toId
  isSource EventAttrs { eventId } (EventSource eid) = eventId == eid
  isSource _ _ = False

instance HasActions env EventAttrs where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env EventAttrs where
  runMessage msg a@EventAttrs {..} = case msg of
    InvestigatorEliminated iid
      | eventAttachedTarget == Just (InvestigatorTarget iid) -> a
      <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEvent eid target | eid == eventId ->
      pure $ a & attachedTargetL ?~ target
    _ -> pure a
