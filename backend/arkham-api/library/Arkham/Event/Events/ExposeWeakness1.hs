module Arkham.Event.Events.ExposeWeakness1 (exposeWeakness1, exposeWeakness1Effect) where

import Arkham.Action qualified as Action
import Arkham.Effect.Import
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers hiding (EnemyFight)
import Arkham.Helpers.Modifiers qualified as Mods
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher

newtype ExposeWeakness1 = ExposeWeakness1 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1 :: EventCard ExposeWeakness1
exposeWeakness1 = event ExposeWeakness1 Cards.exposeWeakness1

instance RunMessage ExposeWeakness1 where
  runMessage msg e@(ExposeWeakness1 attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      enemies <- select $ enemyAtLocationWith iid <> EnemyWithFight
      sid <- getRandom
      chooseTargetM iid enemies \enemy ->
        beginSkillTest sid iid attrs enemy #intellect (EnemyMaybeFieldCalculation enemy EnemyFight)
      pure e
    PassedThisSkillTestBy _ (isSource attrs -> True) n -> do
      getSkillTestTargetedEnemy
        >>= traverse_ (createCardEffect Cards.exposeWeakness1 (Just $ EffectInt n) attrs)
      pure e
    _ -> ExposeWeakness1 <$> liftRunMessage msg attrs

newtype ExposeWeakness1Effect = ExposeWeakness1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exposeWeakness1Effect :: EffectArgs -> ExposeWeakness1Effect
exposeWeakness1Effect = cardEffect ExposeWeakness1Effect Cards.exposeWeakness1

instance HasModifiersFor ExposeWeakness1Effect where
  getModifiersFor (ExposeWeakness1Effect attrs) = do
    case attrs.meta of
      Just (EffectInt n) -> modified_ attrs attrs.target [Mods.EnemyFight (-n)]
      _ -> error "invalid effect metadata"

instance RunMessage ExposeWeakness1Effect where
  runMessage msg e@(ExposeWeakness1Effect attrs) = runQueueT $ case msg of
    PassedSkillTest _ (Just Action.Fight) _ (Initiator target) _ _ | target == attrs.target -> do
      disableReturn e
    FailedSkillTest _ (Just Action.Fight) _ (Initiator target) _ _ | target == attrs.target -> do
      disableReturn e
    EndPhase -> disableReturn e
    _ -> ExposeWeakness1Effect <$> liftRunMessage msg attrs
