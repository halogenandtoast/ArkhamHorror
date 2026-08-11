module Arkham.Homebrew.DarkMatter.Enemies.Lr02Hali (lr02Hali) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scan)
import Arkham.LocationSymbol qualified as LS
import Arkham.Matcher

newtype Lr02Hali = Lr02Hali EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lr02Hali :: EnemyCard Lr02Hali
lr02Hali = enemy Lr02Hali Cards.lr02Hali

instance HasAbilities Lr02Hali where
  getAbilities (Lr02Hali a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDefeated #after You ByAny (be a)

instance RunMessage Lr02Hali where
  runMessage msg e@(Lr02Hali attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scan iid (attrs.ability 1) [LS.Trefoil]
      pure e
    _ -> Lr02Hali <$> liftRunMessage msg attrs
