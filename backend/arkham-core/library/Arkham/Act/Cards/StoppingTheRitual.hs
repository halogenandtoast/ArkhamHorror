module Arkham.Act.Cards.StoppingTheRitual
  ( StoppingTheRitual(..)
  , stoppingTheRitual
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Timing qualified as Timing

newtype StoppingTheRitual = StoppingTheRitual ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stoppingTheRitual :: ActCard StoppingTheRitual
stoppingTheRitual =
  act (3, A) StoppingTheRitual Cards.stoppingTheRitual Nothing

instance HasModifiersFor StoppingTheRitual where
  getModifiersFor (EnemyTarget eid) (StoppingTheRitual a) = do
    isNahab <- eid <=~> enemyIs Enemies.nahab
    pure $ toModifiers a [ CannotMove | isNahab ]
  getModifiersFor _ _ = pure []

instance HasAbilities StoppingTheRitual where
  getAbilities (StoppingTheRitual a) | onSide A a =
    [ mkAbility a 1 $ ForcedAbility $ EnemyDefeated Timing.When Anyone $ enemyIs
      Enemies.nahab
    , restrictedAbility a 2
        (enemyExists $ enemyIs Enemies.nahab <> NotEnemy EnemyWithAnyDoom)
      $ Objective
      $ ForcedAbility AnyWindow
    ]
  getAbilities _ = []

instance RunMessage StoppingTheRitual where
  runMessage msg a@(StoppingTheRitual attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      nahab <- selectJust $ enemyIs Enemies.nahab
      pushAll
        [ CancelNext (toSource attrs) EnemyDefeatedMessage
        , HealAllDamage (toTarget nahab) (toSource attrs)
        , DisengageEnemyFromAll nahab
        , Exhaust (toTarget nahab)
        , CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [DoesNotReadyDuringUpkeep])
          (toSource attrs)
          (toTarget nahab)
        ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId a) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ scenarioResolution 2
      pure a
    _ -> StoppingTheRitual <$> runMessage msg attrs
