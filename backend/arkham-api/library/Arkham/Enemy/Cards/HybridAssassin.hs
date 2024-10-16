module Arkham.Enemy.Cards.HybridAssassin (hybridAssassin, HybridAssassin (..)) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype HybridAssassin = HybridAssassin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hybridAssassin :: EnemyCard HybridAssassin
hybridAssassin = enemy HybridAssassin Cards.hybridAssassin (3, Static 3, 1) (0, 1)

instance HasAbilities HybridAssassin where
  getAbilities (HybridAssassin attrs) =
    extend1 attrs
      $ groupLimit PerRound
      $ mkAbility attrs 1
      $ forced
      $ MovedFromHunter #after (be attrs <> UnengagedEnemy)

instance RunMessage HybridAssassin where
  runMessage msg e@(HybridAssassin attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      isEngaged <- selectAny $ investigatorEngagedWith attrs
      unless isEngaged do
        push $ HunterMove attrs.id
      pure e
    _ -> HybridAssassin <$> liftRunMessage msg attrs
