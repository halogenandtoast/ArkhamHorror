{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Event.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


import Arkham.Types.Trait
import qualified Data.HashMap.Strict as HashMap

data EventAttrs = EventAttrs
  { eventName :: Text
  , eventId :: EventId
  , eventCardCode :: CardCode
  , eventTraits :: HashSet Trait
  , eventAttachedTarget :: Maybe Target
  , eventOwner :: InvestigatorId
  , eventWeakness :: Bool
  , eventDoom :: Int
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''EventAttrs

instance ToJSON EventAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "event"
  toEncoding = genericToEncoding $ aesonOptions $ Just "event"

instance FromJSON EventAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "event"

instance IsCard EventAttrs where
  getCardId = unEventId . eventId
  getCardCode = eventCardCode
  getTraits = eventTraits
  getKeywords = mempty

unshiftEffect
  :: (HasQueue env, MonadReader env m, MonadIO m)
  => EventAttrs
  -> Target
  -> m ()
unshiftEffect attrs target = unshiftMessages
  [ CreateEffect (eventCardCode attrs) Nothing (toSource attrs) target
  , Discard $ toTarget attrs
  ]

baseAttrs :: InvestigatorId -> EventId -> CardCode -> EventAttrs
baseAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player card: " <> unpack (unCardCode cardCode))
          (HashMap.lookup cardCode allPlayerCards)
        $ unEventId eid
  in
    EventAttrs
      { eventName = pcName
      , eventId = eid
      , eventCardCode = pcCardCode
      , eventTraits = pcTraits
      , eventAttachedTarget = Nothing
      , eventWeakness = False
      , eventOwner = iid
      , eventDoom = 0
      }

weaknessAttrs :: InvestigatorId -> EventId -> CardCode -> EventAttrs
weaknessAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          "missing weakness card"
          (HashMap.lookup cardCode allPlayerCards)
        $ unEventId eid
  in
    EventAttrs
      { eventName = pcName
      , eventId = eid
      , eventCardCode = pcCardCode
      , eventTraits = pcTraits
      , eventAttachedTarget = Nothing
      , eventOwner = iid
      , eventWeakness = True
      , eventDoom = 0
      }

instance Entity EventAttrs where
  type EntityId EventAttrs = EventId
  type EntityAttrs EventAttrs = EventAttrs
  toId = eventId
  toAttrs = id

instance NamedEntity EventAttrs where
  toName = mkName . eventName

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
