module Arkham.Enemy.Cards.MargaretLiuBeguilingLoungeSinger (margaretLiuBeguilingLoungeSinger) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype MargaretLiuBeguilingLoungeSinger = MargaretLiuBeguilingLoungeSinger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

margaretLiuBeguilingLoungeSinger :: EnemyCard MargaretLiuBeguilingLoungeSinger
margaretLiuBeguilingLoungeSinger =
  enemy MargaretLiuBeguilingLoungeSinger Cards.margaretLiuBeguilingLoungeSinger (1, Static 3, 5) (1, 1)

instance HasAbilities MargaretLiuBeguilingLoungeSinger where
  getAbilities (MargaretLiuBeguilingLoungeSinger a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ SkillTestResult #after You (SkillTestAt $ LocationWithEnemy $ be a) (SuccessResult $ AtLeast $ Static 2)

instance RunMessage MargaretLiuBeguilingLoungeSinger where
  runMessage msg e@(MargaretLiuBeguilingLoungeSinger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 6
      pure e
    _ -> MargaretLiuBeguilingLoungeSinger <$> liftRunMessage msg attrs
