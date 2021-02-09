module Arkham.Types.Event.Cards.Backstab where


import Arkham.Types.Action
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers

newtype Backstab = Backstab EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab :: InvestigatorId -> EventId -> Backstab
backstab iid uuid = Backstab $ baseAttrs iid uuid "01051"

instance HasModifiersFor env Backstab where
  getModifiersFor (SkillTestSource _ _ source (Just Fight)) (InvestigatorTarget _) (Backstab attrs)
    = pure $ toModifiers attrs [ DamageDealt 2 | isSource attrs source ]
  getModifiersFor _ _ _ = pure []

instance HasActions env Backstab where
  getActions i window (Backstab attrs) = getActions i window attrs

instance (HasQueue env) => RunMessage env Backstab where
  runMessage msg e@(Backstab attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftMessages
        [ ChooseFightEnemy iid (EventSource eid) SkillAgility False
        , Discard (EventTarget eid)
        ]
    _ -> Backstab <$> runMessage msg attrs
