module Arkham.Enemy.Cards.CrazedShoggoth (crazedShoggoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Matcher
import Arkham.Trait

newtype CrazedShoggoth = CrazedShoggoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crazedShoggoth :: EnemyCard CrazedShoggoth
crazedShoggoth =
  enemy CrazedShoggoth Cards.crazedShoggoth (3, Static 6, 4) (2, 2)
    & setSpawnAt (NearestLocationToYou $ LocationWithTrait Altered)

instance HasAbilities CrazedShoggoth where
  getAbilities (CrazedShoggoth a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ InvestigatorDefeated #when (BySource $ SourceIsEnemyAttack (be a)) You

instance RunMessage CrazedShoggoth where
  runMessage msg e@(CrazedShoggoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      kill (attrs.ability 1) iid
      pure e
    _ -> CrazedShoggoth <$> liftRunMessage msg attrs
