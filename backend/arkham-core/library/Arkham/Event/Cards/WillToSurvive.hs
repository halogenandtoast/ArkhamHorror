module Arkham.Event.Cards.WillToSurvive
  ( willToSurvive
  , WillToSurvive(..)
  , willToSurviveEffect
  , WillToSurviveEffect(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Effect.Attrs
import Arkham.Effect.Runner ()
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Message
import Arkham.Target

newtype WillToSurvive = WillToSurvive EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive :: EventCard WillToSurvive
willToSurvive = event WillToSurvive Cards.willToSurvive

instance RunMessage WillToSurvive where
  runMessage msg e@(WillToSurvive attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll
        [ CreateEffect "60512" Nothing (toSource attrs) (InvestigatorTarget iid)
        , Discard (EventTarget eid)
        ]
    _ -> WillToSurvive <$> runMessage msg attrs

newtype WillToSurviveEffect = WillToSurviveEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurviveEffect :: EffectArgs -> WillToSurviveEffect
willToSurviveEffect = WillToSurviveEffect . uncurry4 (baseAttrs "60512")

instance HasModifiersFor WillToSurviveEffect where
  getModifiersFor _ target (WillToSurviveEffect a@EffectAttrs {..})
    | target == effectTarget = pure
      [toModifier a DoNotDrawChaosTokensForSkillChecks]
  getModifiersFor _ _ _ = pure []

instance RunMessage WillToSurviveEffect where
  runMessage msg e@(WillToSurviveEffect attrs) = case msg of
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    EndTurn _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> WillToSurviveEffect <$> runMessage msg attrs
