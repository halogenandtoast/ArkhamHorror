module Arkham.Enemy.Cards.IshimaruHaruko (ishimaruHaruko, IshimaruHaruko (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype IshimaruHaruko = IshimaruHaruko EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ishimaruHaruko :: EnemyCard IshimaruHaruko
ishimaruHaruko = enemy IshimaruHaruko Cards.ishimaruHaruko (6, Static 4, 3) (1, 1)

instance HasAbilities IshimaruHaruko where
  getAbilities (IshimaruHaruko a) =
    extend a [mkAbility a 1 $ forced $ EnemyDealtDamage #after NonAttackDamageEffect (be a) #any]

instance RunMessage IshimaruHaruko where
  runMessage msg e@(IshimaruHaruko attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ drawEncounterCard iid (attrs.ability 1)
      pure e
    _ -> IshimaruHaruko <$> runMessage msg attrs
