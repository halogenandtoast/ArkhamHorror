module Arkham.Event.Cards.Backstab where

import Arkham.Prelude

import Arkham.Action
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Helpers
import Arkham.Event.Runner
import Arkham.Helpers.SkillTest
import Arkham.Message
import Arkham.SkillType
import Arkham.Source
import Arkham.Target

newtype Backstab = Backstab EventAttrs
  deriving anyclass (IsEvent, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab :: EventCard Backstab
backstab = event Backstab Cards.backstab

instance HasModifiersFor Backstab where
  getModifiersFor (InvestigatorTarget _) (Backstab attrs) = do
    mSkillTestSource <- getSkillTestSource
    case mSkillTestSource of
      Just (SkillTestSource _ _ source (Just Fight)) ->
        pure $ toModifiers attrs [ DamageDealt 2 | isSource attrs source ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage Backstab where
  runMessage msg e@(Backstab attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      e <$ pushAll
        [ ChooseFightEnemy
          iid
          (EventSource eid)
          Nothing
          SkillAgility
          mempty
          False
        , discard attrs
        ]
    _ -> Backstab <$> runMessage msg attrs
