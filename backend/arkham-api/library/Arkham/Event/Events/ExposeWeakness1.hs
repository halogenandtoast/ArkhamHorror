module Arkham.Event.Events.ExposeWeakness1 (exposeWeakness1, exposeWeakness1Effect, ExposeWeakness1 (..)) where

import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers hiding (EnemyFight)
import Arkham.Helpers.Modifiers qualified as Mods
import Arkham.Matcher
import Arkham.Prelude

newtype ExposeWeakness1 = ExposeWeakness1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1 :: EventCard ExposeWeakness1
exposeWeakness1 = event ExposeWeakness1 Cards.exposeWeakness1

instance RunMessage ExposeWeakness1 where
  runMessage msg e@(ExposeWeakness1 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyWithFight
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ targetLabel
            enemy
            [beginSkillTest sid iid attrs enemy #intellect (EnemyMaybeFieldCalculation enemy EnemyFight)]
          | enemy <- enemies
          ]
      pure e
    PassedThisSkillTestBy _ (isSource attrs -> True) n -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Just (EnemyTarget enemyId) ->
          push =<< createCardEffect Cards.exposeWeakness1 (Just $ EffectInt n) attrs enemyId
        _ -> error "had to have an enemy"
      pure e
    _ -> ExposeWeakness1 <$> runMessage msg attrs

newtype ExposeWeakness1Effect = ExposeWeakness1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1Effect :: EffectArgs -> ExposeWeakness1Effect
exposeWeakness1Effect = cardEffect ExposeWeakness1Effect Cards.exposeWeakness1

instance HasModifiersFor ExposeWeakness1Effect where
  getModifiersFor (ExposeWeakness1Effect attrs) = do
    case effectMetadata attrs of
      Just (EffectInt n) -> modified_ attrs attrs.target [Mods.EnemyFight (-n)]
      _ -> error "invalid effect metadata"

instance RunMessage ExposeWeakness1Effect where
  runMessage msg e@(ExposeWeakness1Effect attrs) = case msg of
    PassedSkillTest _ (Just Action.Fight) _ (Initiator target) _ _ | target == attrs.target -> do
      push $ disable attrs
      pure e
    FailedSkillTest _ (Just Action.Fight) _ (Initiator target) _ _ | target == attrs.target -> do
      push $ disable attrs
      pure e
    EndPhase -> do
      push $ disable attrs
      pure e
    _ -> ExposeWeakness1Effect <$> runMessage msg attrs
