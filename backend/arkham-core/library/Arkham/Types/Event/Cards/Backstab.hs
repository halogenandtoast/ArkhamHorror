{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Event.Cards.Backstab where

import Arkham.Import

import Arkham.Types.Event.Attrs

newtype Backstab = Backstab Attrs
  deriving newtype (Show, ToJSON, FromJSON)

backstab :: InvestigatorId -> EventId -> Backstab
backstab iid uuid = Backstab $ baseAttrs iid uuid "01051"

instance HasModifiersFor env Backstab where
  getModifiersFor _ _ _ = pure []

instance HasActions env Backstab where
  getActions i window (Backstab attrs) = getActions i window attrs

instance (HasQueue env) => RunMessage env Backstab where
  runMessage msg (Backstab attrs@Attrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      unshiftMessages
        [ ChooseFightEnemy
          iid
          (EventSource eid)
          SkillAgility
          [DamageDealt 2]
          mempty
          False
        , Discard (EventTarget eid)
        ]
      Backstab <$> runMessage msg (attrs & resolved .~ True)
    _ -> Backstab <$> runMessage msg attrs
