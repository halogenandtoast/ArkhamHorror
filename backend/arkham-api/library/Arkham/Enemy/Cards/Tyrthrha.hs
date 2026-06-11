module Arkham.Enemy.Cards.Tyrthrha (tyrthrha) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.MachinationsThroughTime.Helpers
import Arkham.Trait (Trait (Scientist))

newtype Tyrthrha = Tyrthrha EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tyrthrha :: EnemyCard Tyrthrha
tyrthrha = enemy Tyrthrha Cards.tyrthrha (3, PerPlayer 6, 3) (1, 2)

instance HasAbilities Tyrthrha where
  getAbilities (Tyrthrha a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEnters #when Anywhere (be a)

instance RunMessage Tyrthrha where
  runMessage msg e@(Tyrthrha attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      withLocationOf attrs \lid -> do
        scientists <- select $ AssetWithTrait Scientist <> AssetAt (LocationWithId lid)
        for_ scientists abductById
      pure e
    _ -> Tyrthrha <$> liftRunMessage msg attrs
