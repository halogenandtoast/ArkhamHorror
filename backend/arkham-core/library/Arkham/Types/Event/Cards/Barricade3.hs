{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Barricade3 where

import Arkham.Import

import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype Barricade3 = Barricade3 Attrs
  deriving newtype (Show, ToJSON, FromJSON)

barricade3 :: InvestigatorId -> EventId -> Barricade3
barricade3 iid uuid = Barricade3 $ baseAttrs iid uuid "50004"

instance HasModifiersFor env Barricade3 where
  getModifiersFor _ _ _ = pure []

instance HasActions env Barricade3 where
  getActions i window (Barricade3 attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env Barricade3 where
  runMessage msg e@(Barricade3 attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- asks (getId iid)
      e <$ unshiftMessage (AttachEventToLocation eid lid)
    MoveFrom _ lid | Just lid == eventAttachedLocation ->
      e <$ unshiftMessage (Discard (EventTarget eventId))
    AttachEventToLocation eid lid | eid == eventId -> do
      unshiftMessage $ AddModifiers
        (LocationTarget lid)
        (EventSource eid)
        [CannotBeEnteredByNonElite, SpawnNonEliteAtConnectingInstead]
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
