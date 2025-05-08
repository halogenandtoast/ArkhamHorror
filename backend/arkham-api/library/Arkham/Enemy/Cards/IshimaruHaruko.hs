module Arkham.Enemy.Cards.IshimaruHaruko (ishimaruHaruko) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype IshimaruHaruko = IshimaruHaruko EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ishimaruHaruko :: EnemyCard IshimaruHaruko
ishimaruHaruko = enemy IshimaruHaruko Cards.ishimaruHaruko (6, Static 4, 3) (1, 1)

instance HasAbilities IshimaruHaruko where
  getAbilities (IshimaruHaruko a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyDealtDamage #after NonAttackDamageEffect (be a) #any

instance RunMessage IshimaruHaruko where
  runMessage msg e@(IshimaruHaruko attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawEncounterCard iid (attrs.ability 1)
      pure e
    _ -> IshimaruHaruko <$> liftRunMessage msg attrs
