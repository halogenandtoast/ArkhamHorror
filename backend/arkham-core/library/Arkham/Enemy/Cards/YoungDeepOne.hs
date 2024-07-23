module Arkham.Enemy.Cards.YoungDeepOne (YoungDeepOne (..), youngDeepOne) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype YoungDeepOne = YoungDeepOne EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youngDeepOne :: EnemyCard YoungDeepOne
youngDeepOne =
  enemyWith YoungDeepOne Cards.youngDeepOne (3, Static 3, 3) (1, 1)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #combat)

instance HasAbilities YoungDeepOne where
  getAbilities (YoungDeepOne a) = extend a [forcedAbility a 1 $ EnemyEngaged #after You (be a)]

instance RunMessage YoungDeepOne where
  runMessage msg e@(YoungDeepOne attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid attrs 1
      pure e
    _ -> YoungDeepOne <$> liftRunMessage msg attrs
