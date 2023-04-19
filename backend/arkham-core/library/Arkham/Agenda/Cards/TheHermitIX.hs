module Arkham.Agenda.Cards.TheHermitIX
  ( TheHermitIX(..)
  , theHermitIX
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Cost
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Timing qualified as Timing

newtype TheHermitIX = TheHermitIX AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theHermitIX :: AgendaCard TheHermitIX
theHermitIX = agenda (1, A) TheHermitIX Cards.theHermitIX (Static 4)

instance HasModifiersFor TheHermitIX where
  getModifiersFor (EnemyTarget eid) (TheHermitIX a) = do
    isNonWeakness <- eid <=~> NonWeaknessEnemy
    pure $ toModifiers a [HealthModifier 1 | isNonWeakness]
  getModifiersFor _ _ = pure []

instance HasAbilities TheHermitIX where
  getAbilities (TheHermitIX a) = [mkAbility a 1 $ ReactionAbility (EnemyDefeated Timing.After You $ EnemyOneOf [enemyIs Enemies.nahab, enemyIs Enemies.brownJenkin]) Free]

instance RunMessage TheHermitIX where
  runMessage msg a@(TheHermitIX attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)]
    _ -> TheHermitIX <$> runMessage msg attrs
