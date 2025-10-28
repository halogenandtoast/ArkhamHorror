module Arkham.Agenda.Cards.TheRedDepths (theRedDepthsEffect, theRedDepths) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Matcher
import Arkham.Scenarios.TheDepthsOfYoth.Helpers

newtype TheRedDepths = TheRedDepths AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedDepths :: AgendaCard TheRedDepths
theRedDepths = agenda (6, A) TheRedDepths Cards.theRedDepths (Static 5)

instance HasAbilities TheRedDepths where
  getAbilities (TheRedDepths a) =
    [ restricted a 1 (OutOfPlayEnemyExists PursuitZone AnyEnemy)
        $ forced
        $ PlacedCounterOnAgenda #after (AgendaWithSide A) AnySource DoomCounter (AtLeast $ Static 1)
    ]

instance RunMessage TheRedDepths where
  runMessage msg a@(TheRedDepths attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      placePursuitEnemies
      card <- flipCard <$> genCard (toCardDef attrs)
      push $ PlaceNextTo ActDeckTarget [card]
      createCardEffect Cards.theRedDepths Nothing attrs ScenarioTarget
      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placePursuitEnemies
      pure a
    _ -> TheRedDepths <$> liftRunMessage msg attrs

newtype TheRedDepthsEffect = TheRedDepthsEffect EffectAttrs
  deriving anyclass (HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theRedDepthsEffect :: EffectArgs -> TheRedDepthsEffect
theRedDepthsEffect = cardEffect TheRedDepthsEffect Cards.theRedDepths

instance HasAbilities TheRedDepthsEffect where
  getAbilities (TheRedDepthsEffect attrs) =
    [ restricted
        (proxied (AgendaMatcherSource AnyAgenda) attrs)
        1
        (OutOfPlayEnemyExists PursuitZone AnyEnemy)
        $ forced
        $ PlacedCounterOnAgenda #after (AgendaWithSide A) AnySource DoomCounter (AtLeast $ Static 1)
    ]

instance RunMessage TheRedDepthsEffect where
  runMessage msg e@(TheRedDepthsEffect attrs) = runQueueT $ case msg of
    UseCardAbility _ (ProxySource _ (isSource attrs -> True)) 1 _ _ -> do
      placePursuitEnemies
      pure e
    _ -> TheRedDepthsEffect <$> liftRunMessage msg attrs
