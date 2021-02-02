{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Event.Attrs where

import Arkham.Import

import Arkham.Types.Trait
import qualified Data.HashMap.Strict as HashMap

data Attrs = Attrs
  { eventName :: Text
  , eventId :: EventId
  , eventCardCode :: CardCode
  , eventTraits :: HashSet Trait
  , eventAttachedTarget :: Maybe Target
  , eventOwner :: InvestigatorId
  , eventWeakness :: Bool
  , eventDoom :: Int
  }
  deriving stock (Show, Generic)

makeLensesWith suffixedFields ''Attrs

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "event"
  toEncoding = genericToEncoding $ aesonOptions $ Just "event"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "event"

instance IsCard Attrs where
  getCardId = CardId . unEventId . eventId
  getCardCode = eventCardCode
  getTraits = eventTraits
  getKeywords = mempty

unshiftEffect
  :: (HasQueue env, MonadReader env m, MonadIO m) => Attrs -> Target -> m ()
unshiftEffect attrs target = unshiftMessages
  [ CreateEffect (eventCardCode attrs) Nothing (toSource attrs) target
  , Discard $ toTarget attrs
  ]

baseAttrs :: InvestigatorId -> EventId -> CardCode -> Attrs
baseAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player card: " <> unpack (unCardCode cardCode))
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unEventId eid)
  in
    Attrs
      { eventName = pcName
      , eventId = eid
      , eventCardCode = pcCardCode
      , eventTraits = pcTraits
      , eventAttachedTarget = Nothing
      , eventWeakness = False
      , eventOwner = iid
      , eventDoom = 0
      }

weaknessAttrs :: InvestigatorId -> EventId -> CardCode -> Attrs
weaknessAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          "missing weakness card"
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unEventId eid)
  in
    Attrs
      { eventName = pcName
      , eventId = eid
      , eventCardCode = pcCardCode
      , eventTraits = pcTraits
      , eventAttachedTarget = Nothing
      , eventOwner = iid
      , eventWeakness = True
      , eventDoom = 0
      }

instance Entity Attrs where
  type EntityId Attrs = EventId
  type EntityAttrs Attrs = Attrs
  toId = eventId
  toAttrs = id
  toSource = EventSource . toId
  toTarget = EventTarget . toId
  isSource Attrs { eventId } (EventSource eid) = eventId == eid
  isSource _ _ = False
  isTarget Attrs { eventId } (EventTarget eid) = eventId == eid
  isTarget attrs (SkillTestInitiatorTarget target) = isTarget attrs target
  isTarget _ _ = False

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorEliminated iid
      | eventAttachedTarget == Just (InvestigatorTarget iid) -> a
      <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEvent eid target | eid == eventId ->
      pure $ a & attachedTargetL ?~ target
    _ -> pure a
