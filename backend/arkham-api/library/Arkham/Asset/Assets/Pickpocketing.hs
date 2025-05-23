module Arkham.Asset.Assets.Pickpocketing (pickpocketing) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyEvaded)
import Arkham.Matcher

newtype Pickpocketing = Pickpocketing AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

pickpocketing :: AssetCard Pickpocketing
pickpocketing = asset Pickpocketing Cards.pickpocketing

instance HasAbilities Pickpocketing where
  getAbilities (Pickpocketing a) =
    [restricted a 1 ControlsThis $ triggered (EnemyEvaded #after You AnyEnemy) (exhaust a)]

instance RunMessage Pickpocketing where
  runMessage msg a@(Pickpocketing attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCards iid (attrs.ability 1) 1
      pure a
    _ -> Pickpocketing <$> liftRunMessage msg attrs
