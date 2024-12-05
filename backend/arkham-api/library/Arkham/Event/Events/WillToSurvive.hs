module Arkham.Event.Events.WillToSurvive (
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
    PlayThisEvent iid eid | eid == attrs.id -> do
      push =<< createCardEffect Cards.willToSurvive Nothing attrs iid
      pure e
    _ -> WillToSurvive <$> runMessage msg attrs

newtype WillToSurviveEffect = WillToSurviveEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurviveEffect :: EffectArgs -> WillToSurviveEffect
willToSurviveEffect = cardEffect WillToSurviveEffect Cards.willToSurvive

instance HasModifiersFor WillToSurviveEffect where
  getModifiersFor (WillToSurviveEffect a) = do
    modified_ a a.target [DoNotDrawChaosTokensForSkillChecks]

instance RunMessage WillToSurviveEffect where
  runMessage msg e@(WillToSurviveEffect attrs) = case msg of
    SkillTestEnds _ _ _ -> e <$ push (DisableEffect $ effectId attrs)
    EndTurn _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WillToSurviveEffect <$> runMessage msg attrs
