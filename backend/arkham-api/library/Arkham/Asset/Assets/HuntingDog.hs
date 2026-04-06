module Arkham.Asset.Assets.HuntingDog (huntingDog) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Window (enteringEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

newtype HuntingDog = HuntingDog AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntingDog :: AssetCard HuntingDog
huntingDog = ally HuntingDog Cards.huntingDog (1, 1)

instance HasAbilities HuntingDog where
  getAbilities (HuntingDog a) =
    [ controlled_ a 1
        $ triggered
          (EnemyEnters #when (CanMoveCloserToLocation (a.ability 1) You Anywhere) AnyEnemy)
          (exhaust a)
    ]

instance RunMessage HuntingDog where
  runMessage msg a@(HuntingDog attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (enteringEnemy -> eid) _ -> do
      withLocationOf eid $ moveToward iid
      pure a
    _ -> HuntingDog <$> liftRunMessage msg attrs
