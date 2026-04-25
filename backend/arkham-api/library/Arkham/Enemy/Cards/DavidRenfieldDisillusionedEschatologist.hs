module Arkham.Enemy.Cards.DavidRenfieldDisillusionedEschatologist (davidRenfieldDisillusionedEschatologist) where

import Arkham.Ability
import Arkham.Campaigns.BrethrenOfAsh.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype DavidRenfieldDisillusionedEschatologist = DavidRenfieldDisillusionedEschatologist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

davidRenfieldDisillusionedEschatologist :: EnemyCard DavidRenfieldDisillusionedEschatologist
davidRenfieldDisillusionedEschatologist =
  enemy DavidRenfieldDisillusionedEschatologist Cards.davidRenfieldDisillusionedEschatologist (1, Static 5, 4) (1, 1)

instance HasAbilities DavidRenfieldDisillusionedEschatologist where
  getAbilities (DavidRenfieldDisillusionedEschatologist a) =
    extend1 a
      $ mkAbility a 1
      $ freeReaction
      $ SkillTestResult #after You (WhileInvestigating $ LocationWithEnemy $ be a) (SuccessResult AnyValue)

instance RunMessage DavidRenfieldDisillusionedEschatologist where
  runMessage msg e@(DavidRenfieldDisillusionedEschatologist attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      codex iid (attrs.ability 1) 1
      pure e
    _ -> DavidRenfieldDisillusionedEschatologist <$> liftRunMessage msg attrs
