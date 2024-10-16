module Arkham.Enemy.Cards.TheTerrorOfDevilReefRelentlessMonstrosity (
  theTerrorOfDevilReefRelentlessMonstrosity,
  TheTerrorOfDevilReefRelentlessMonstrosity (..),
)
where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher
import Arkham.Modifier

newtype TheTerrorOfDevilReefRelentlessMonstrosity = TheTerrorOfDevilReefRelentlessMonstrosity EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTerrorOfDevilReefRelentlessMonstrosity :: EnemyCard TheTerrorOfDevilReefRelentlessMonstrosity
theTerrorOfDevilReefRelentlessMonstrosity =
  enemyWith
    TheTerrorOfDevilReefRelentlessMonstrosity
    Cards.theTerrorOfDevilReefRelentlessMonstrosity
    (3, Static 6, 3)
    (2, 2)
    (spawnAtL ?~ SpawnAt RearmostLocation)

instance HasAbilities TheTerrorOfDevilReefRelentlessMonstrosity where
  getAbilities (TheTerrorOfDevilReefRelentlessMonstrosity a) =
    extend a [mkAbility a 1 $ forced $ EnemyDefeated #when Anyone ByAny (be a)]

instance RunMessage TheTerrorOfDevilReefRelentlessMonstrosity where
  runMessage msg e@(TheTerrorOfDevilReefRelentlessMonstrosity attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      insteadOfDefeat attrs do
        healAllDamage (attrs.ability 1) attrs
        exhaustThis attrs
        roundModifier (attrs.ability 1) attrs CannotReady
      pure e
    _ -> TheTerrorOfDevilReefRelentlessMonstrosity <$> liftRunMessage msg attrs
