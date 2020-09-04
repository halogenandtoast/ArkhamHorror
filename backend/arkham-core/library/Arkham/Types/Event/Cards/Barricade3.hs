{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Barricade3 where

import Arkham.Json
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import Lens.Micro
import Safe (fromJustNote)

newtype Barricade3 = Barricade3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

barricade3 :: InvestigatorId -> EventId -> Barricade3
barricade3 iid uuid = Barricade3 $ baseAttrs iid uuid "50004"

instance HasActions env investigator Barricade3 where
  getActions i window (Barricade3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Barricade3 where
  runMessage msg e@(Barricade3 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      lid <- asks (getId iid)
      e <$ unshiftMessage (AttachEventToLocation eid lid)
    MoveFrom _ lid | Just lid == eventAttachedLocation ->
      e <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEventToLocation eid lid | eid == eventId -> do
      unshiftMessages
        [ AddModifier
          (LocationTarget lid)
          (CannotBeEnteredByNonElite (EventSource eid))
        , AddModifier
          (LocationTarget lid)
          (SpawnNonEliteAtConnectingInstead (EventSource eid))
        ]
      pure . Barricade3 $ attrs & attachedLocation ?~ lid
    Discard (EventTarget eid) | eid == eventId -> do
      unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
            (LocationTarget
            $ fromJustNote "had to have been attached" eventAttachedLocation
            )
            (EventSource eventId)
        ]
      Barricade3 <$> runMessage msg attrs
    _ -> Barricade3 <$> runMessage msg attrs
