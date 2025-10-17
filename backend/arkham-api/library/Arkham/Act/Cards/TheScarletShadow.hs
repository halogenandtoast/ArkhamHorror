module Arkham.Act.Cards.TheScarletShadow (theScarletShadow) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheScarletShadow = TheScarletShadow ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theScarletShadow :: ActCard TheScarletShadow
theScarletShadow = act (1, A) TheScarletShadow Cards.theScarletShadow Nothing

instance HasAbilities TheScarletShadow where
  getAbilities = actAbilities \a ->
    [ mkAbility a 1
        $ Objective
        $ forced
        $ EnemyEngaged #after Anyone (enemyIs Enemies.laChicaRojaTheGirlInTheCarmineCoat)
    ]

instance RunMessage TheScarletShadow where
  runMessage msg a@(TheScarletShadow attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> TheScarletShadow <$> liftRunMessage msg attrs
