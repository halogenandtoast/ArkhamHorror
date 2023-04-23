module Arkham.Agenda.Cards.TheRedDepths
  ( TheRedDepths(..)
  , theRedDepthsEffect
  , theRedDepths
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Source
import Arkham.Timing qualified as Timing

newtype TheRedDepths = TheRedDepths AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedDepths :: AgendaCard TheRedDepths
theRedDepths = agenda (6, A) TheRedDepths Cards.theRedDepths (Static 5)

instance HasAbilities TheRedDepths where
  getAbilities (TheRedDepths a) =
    [ restrictedAbility a 1 (OutOfPlayEnemyExists PursuitZone AnyEnemy)
        $ ForcedAbility
        $ PlacedCounterOnAgenda
            Timing.After
            (AgendaWithSide A)
            DoomCounter
            (AtLeast $ Static 1)
    ]

instance RunMessage TheRedDepths where
  runMessage msg a@(TheRedDepths attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      enemyMsgs <- getPlacePursuitEnemyMessages
      card <- flipCard <$> genCard (toCardDef attrs)
      pushAll
        $ enemyMsgs
        <> [ PlaceNextTo ActDeckTarget [card]
           , createCardEffect
             Cards.theRedDepths
             Nothing
             (toSource attrs)
             ScenarioTarget
           , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
           ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      enemyMsgs <- getPlacePursuitEnemyMessages
      pushAll enemyMsgs
      pure a
    _ -> TheRedDepths <$> runMessage msg attrs

newtype TheRedDepthsEffect = TheRedDepthsEffect EffectAttrs
  deriving anyclass (HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedDepthsEffect :: EffectArgs -> TheRedDepthsEffect
theRedDepthsEffect = cardEffect TheRedDepthsEffect Cards.theRedDepths

instance HasAbilities TheRedDepthsEffect where
  getAbilities (TheRedDepthsEffect attrs) =
    [ restrictedAbility
          (ProxySource (AgendaMatcherSource AnyAgenda) (toSource attrs))
          1
          (OutOfPlayEnemyExists PursuitZone AnyEnemy)
        $ ForcedAbility
        $ PlacedCounterOnAgenda
            Timing.After
            (AgendaWithSide A)
            DoomCounter
            (AtLeast $ Static 1)
    ]

instance RunMessage TheRedDepthsEffect where
  runMessage msg e@(TheRedDepthsEffect attrs) = case msg of
    UseCardAbility _ (ProxySource _ (isSource attrs -> True)) 1 _ _ -> do
      enemyMsgs <- getPlacePursuitEnemyMessages
      pushAll enemyMsgs
      pure e
    _ -> TheRedDepthsEffect <$> runMessage msg attrs
