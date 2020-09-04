{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Barricade where

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

newtype Barricade = Barricade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

barricade :: InvestigatorId -> EventId -> Barricade
barricade iid uuid = Barricade $ baseAttrs iid uuid "01038"

instance HasActions env investigator Barricade where
  getActions i window (Barricade attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Barricade where
  runMessage msg e@(Barricade attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid | eid == eventId -> do
      lid <- asks (getId iid)
      e <$ unshiftMessage (AttachEventToLocation eid lid)
    MoveFrom _ lid | Just lid == eventAttachedLocation ->
      e <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEventToLocation eid lid | eid == eventId -> do
      unshiftMessage
        (AddModifier
          (LocationTarget lid)
          (EventSource eid)
          CannotBeEnteredByNonElite
        )
      pure . Barricade $ attrs & attachedLocation ?~ lid
    Discard (EventTarget eid) | eid == eventId -> do
      unshiftMessages
        [ RemoveAllModifiersOnTargetFrom
            (LocationTarget
            $ fromJustNote "had to have been attached" eventAttachedLocation
            )
            (EventSource eventId)
        ]
      Barricade <$> runMessage msg attrs
    _ -> Barricade <$> runMessage msg attrs
