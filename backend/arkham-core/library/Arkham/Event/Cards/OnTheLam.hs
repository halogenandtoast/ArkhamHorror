module Arkham.Event.Cards.OnTheLam where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
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
    PlayThisEvent iid eid | attrs `is` eid -> do
      unshiftEffect attrs iid
      pure e
    _ -> OnTheLam <$> runMessage msg attrs

newtype OnTheLamEffect = OnTheLamEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onTheLamEffect :: EffectArgs -> OnTheLamEffect
onTheLamEffect = cardEffect OnTheLamEffect Cards.onTheLam

instance HasModifiersFor OnTheLamEffect where
  getModifiersFor target (OnTheLamEffect a) | a.target `is` target = do
    pure $ toModifiers a [CannotBeAttackedBy NonEliteEnemy]
  getModifiersFor _ _ = pure []

instance RunMessage OnTheLamEffect where
  runMessage msg e@(OnTheLamEffect attrs) = case msg of
    EndRound -> do
      push $ disable attrs
      pure e
    _ -> OnTheLamEffect <$> runMessage msg attrs
