module Arkham.Agenda.Cards.MarkedForSacrifice (
  MarkedForSacrifice (..),
  markedForSacrifice,
) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Timing qualified as Timing

newtype MarkedForSacrifice = MarkedForSacrifice AgendaAttrs
  deriving anyclass (IsAgenda)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

markedForSacrifice :: AgendaCard MarkedForSacrifice
markedForSacrifice =
  agenda (4, A) MarkedForSacrifice Cards.markedForSacrifice (Static 8)

instance HasModifiersFor MarkedForSacrifice where
  getModifiersFor (EnemyTarget eid) (MarkedForSacrifice a) = do
    isNonWeakness <- eid <=~> NonWeaknessEnemy
    pure $ toModifiers a [HealthModifier 4 | isNonWeakness]
  getModifiersFor _ _ = pure []

instance HasAbilities MarkedForSacrifice where
  getAbilities (MarkedForSacrifice a) =
    [ mkAbility a 1
        $ ReactionAbility
          ( EnemyDefeated Timing.After You ByAny
              $ EnemyOneOf [enemyIs Enemies.nahab, enemyIs Enemies.brownJenkin]
          )
          Free
    ]

instance RunMessage MarkedForSacrifice where
  runMessage msg a@(MarkedForSacrifice attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      push $ scenarioResolution 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      playerCount <- getPlayerCount
      push $ GainClues iid (toAbilitySource attrs 1) $ if playerCount >= 3 then 2 else 1
      pure a
    _ -> MarkedForSacrifice <$> runMessage msg attrs
