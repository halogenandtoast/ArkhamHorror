module Arkham.Types.Event.Cards.Backstab where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Backstab = Backstab EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab :: EventCard Backstab
backstab = event Backstab Cards.backstab

instance HasModifiersFor env Backstab where
  getModifiersFor (SkillTestSource _ _ source _ (Just Fight)) (InvestigatorTarget _) (Backstab attrs)
    = pure $ toModifiers attrs [ DamageDealt 2 | isSource attrs source ]
  getModifiersFor _ _ _ = pure []

instance HasActions env Backstab where
  getActions i window (Backstab attrs) = getActions i window attrs

instance (HasQueue env) => RunMessage env Backstab where
  runMessage msg e@(Backstab attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId -> do
      e <$ pushAll
        [ ChooseFightEnemy iid (EventSource eid) SkillAgility mempty False
        , Discard (EventTarget eid)
        ]
    _ -> Backstab <$> runMessage msg attrs
