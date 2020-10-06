{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Attrs where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (playerCardAttrs)
import qualified Arkham.Types.Card.PlayerCard.Attrs as PlayerCard
import Arkham.Types.Classes
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Lens.Micro
import Safe (fromJustNote)

data Attrs = Attrs
  { eventName :: Text
  , eventId :: EventId
  , eventCardCode :: CardCode
  , eventTraits :: HashSet Trait
  , eventAttachedLocation :: Maybe LocationId
  , eventAttachedInvestigator :: Maybe InvestigatorId
  , eventOwner :: InvestigatorId
  , eventWeakness :: Bool
  , eventResolved :: Bool -- should this be discarded
  , eventDoom :: Int
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "event"
  toEncoding = genericToEncoding $ aesonOptions $ Just "event"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "event"

instance HasId EventId () Attrs where
  getId _ Attrs {..} = eventId

resolved :: Lens' Attrs Bool
resolved = lens eventResolved $ \m x -> m { eventResolved = x }

attachedLocation :: Lens' Attrs (Maybe LocationId)
attachedLocation =
  lens eventAttachedLocation $ \m x -> m { eventAttachedLocation = x }

baseAttrs :: InvestigatorId -> EventId -> CardCode -> Attrs
baseAttrs iid eid cardCode =
  let
    PlayerCard.Attrs {..} =
      playerCardAttrs
        . fromJustNote
            ("missing player card: " <> unpack (unCardCode cardCode))
            (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unEventId eid)
  in
    Attrs
      { eventName = pcName
      , eventId = eid
      , eventCardCode = pcCardCode
      , eventTraits = HashSet.fromList pcTraits
      , eventAttachedLocation = Nothing
      , eventAttachedInvestigator = Nothing
      , eventWeakness = False
      , eventResolved = False
      , eventOwner = iid
      , eventDoom = 0
      }

weaknessAttrs :: InvestigatorId -> EventId -> CardCode -> Attrs
weaknessAttrs iid eid cardCode =
  let
    PlayerCard.Attrs {..} =
      playerCardAttrs
        . fromJustNote
            "missing weakness card"
            (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unEventId eid)
  in
    Attrs
      { eventName = pcName
      , eventId = eid
      , eventCardCode = pcCardCode
      , eventTraits = HashSet.fromList pcTraits
      , eventAttachedLocation = Nothing
      , eventAttachedInvestigator = Nothing
      , eventOwner = iid
      , eventWeakness = True
      , eventResolved = False
      , eventDoom = 0
      }

instance HasActions env investigator Attrs where
  getActions _ _ _ = pure []

instance HasQueue env => RunMessage env Attrs where
  runMessage msg a@Attrs {..} = case msg of
    InvestigatorEliminated iid | eventAttachedInvestigator == Just iid ->
      a <$ unshiftMessage (Discard (EventTarget eventId))
    _ -> pure a
