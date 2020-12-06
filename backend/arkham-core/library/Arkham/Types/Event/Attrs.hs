{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Attrs where

import Arkham.Import

import Arkham.Types.Trait
import qualified Data.HashMap.Strict as HashMap

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

data Attrs = Attrs
  { eventName :: Text
  , eventId :: EventId
  , eventCardCode :: CardCode
  , eventTraits :: HashSet Trait
  , eventAttachedLocation :: Maybe LocationId
  , eventAttachedInvestigator :: Maybe InvestigatorId
  , eventOwner :: InvestigatorId
  , eventWeakness :: Bool
  , eventDoom :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "event"
  toEncoding = genericToEncoding $ aesonOptions $ Just "event"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "event"

instance HasId EventId env Attrs where
  getId = pure . eventId

unshiftEffect
  :: (HasQueue env, MonadReader env m, MonadIO m) => Attrs -> Target -> m ()
unshiftEffect attrs target = unshiftMessages
  [ CreateEffect (eventCardCode attrs) Nothing (toSource attrs) target
  , Discard $ toTarget attrs
  ]

attachedLocation :: Lens' Attrs (Maybe LocationId)
attachedLocation =
  lens eventAttachedLocation $ \m x -> m { eventAttachedLocation = x }

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
      , eventAttachedLocation = Nothing
      , eventAttachedInvestigator = Nothing
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
      , eventAttachedLocation = Nothing
      , eventAttachedInvestigator = Nothing
      , eventOwner = iid
      , eventWeakness = True
      , eventDoom = 0
      }

instance Entity Attrs where
  type EntityId Attrs = EventId
  toId = eventId
  toSource = EventSource . toId
  toTarget = EventTarget . toId
  isSource Attrs { eventId } (EventSource eid) = eventId == eid
  isSource _ _ = False
  isTarget Attrs { eventId } (EventTarget eid) = eventId == eid
  isTarget _ _ = False

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorEliminated iid | eventAttachedInvestigator == Just iid ->
      a <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEventToLocation eid lid | eid == eventId ->
      pure $ a & attachedLocation ?~ lid
    _ -> pure a
