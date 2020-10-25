{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Barricade where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Barricade = Barricade Attrs
  deriving newtype (Show, ToJSON, FromJSON)

barricade :: InvestigatorId -> EventId -> Barricade
barricade iid uuid = Barricade $ baseAttrs iid uuid "01038"

instance HasModifiersFor env Barricade where
  getModifiersFor _ _ _ = pure []

instance HasActions env Barricade where
  getActions i window (Barricade attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Barricade where
  runMessage msg e@(Barricade attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- asks (getId iid)
      e <$ unshiftMessage (AttachEventToLocation eid lid)
    MoveFrom _ lid | Just lid == eventAttachedLocation ->
      e <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEventToLocation eid lid | eid == eventId -> do
      unshiftMessage
        (AddModifiers
          (LocationTarget lid)
          (EventSource eid)
          [CannotBeEnteredByNonElite]
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
