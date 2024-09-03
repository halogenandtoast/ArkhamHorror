module Arkham.Event.Cards.WillToSurvive (
  willToSurvive,
  WillToSurvive (..),
  willToSurviveEffect,
  WillToSurviveEffect (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers

newtype WillToSurvive = WillToSurvive EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive :: EventCard WillToSurvive
willToSurvive = event WillToSurvive Cards.willToSurvive

instance RunMessage WillToSurvive where
  runMessage msg e@(WillToSurvive attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      push $ createCardEffect Cards.willToSurvive Nothing (toSource attrs) (InvestigatorTarget iid)
      pure e
    _ -> WillToSurvive <$> runMessage msg attrs

newtype WillToSurviveEffect = WillToSurviveEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurviveEffect :: EffectArgs -> WillToSurviveEffect
willToSurviveEffect = cardEffect WillToSurviveEffect Cards.willToSurvive

instance HasModifiersFor WillToSurviveEffect where
  getModifiersFor target (WillToSurviveEffect a) | target == effectTarget a = do
    pure [toModifier a DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ = pure []

instance RunMessage WillToSurviveEffect where
  runMessage msg e@(WillToSurviveEffect attrs) = case msg of
    SkillTestEnds _ _ _ -> e <$ push (DisableEffect $ effectId attrs)
    EndTurn _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WillToSurviveEffect <$> runMessage msg attrs
