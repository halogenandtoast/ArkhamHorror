module Arkham.Event.Cards.OnTheLam where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Event
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Message

newtype OnTheLam = OnTheLam EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLam :: EventCard OnTheLam
onTheLam = event OnTheLam Cards.onTheLam

instance RunMessage OnTheLam where
  runMessage msg e@(OnTheLam attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      unshiftEffect attrs iid
      pure e
    _ -> OnTheLam <$> runMessage msg attrs

newtype OnTheLamEffect = OnTheLamEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLamEffect :: EffectArgs -> OnTheLamEffect
onTheLamEffect = cardEffect OnTheLamEffect Cards.onTheLam

instance HasModifiersFor OnTheLamEffect where
  getModifiersFor target (OnTheLamEffect a) | target == effectTarget a = do
    pure $ toModifiers a [CannotBeAttackedBy NonEliteEnemy]
  getModifiersFor _ _ = pure []

instance RunMessage OnTheLamEffect where
  runMessage msg e@(OnTheLamEffect attrs) = case msg of
    EndRound -> do
      push $ DisableEffect $ toId attrs
      pure e
    _ -> OnTheLamEffect <$> runMessage msg attrs
