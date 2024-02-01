module Arkham.Asset.Cards.Burglary2 (
  burglary2,
  Burglary2 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate

newtype Burglary2 = Burglary2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

burglary2 :: AssetCard Burglary2
burglary2 = asset Burglary2 Cards.burglary2

instance HasAbilities Burglary2 where
  getAbilities (Burglary2 a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Burglary2 where
  runMessage msg a@(Burglary2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) (min 3 -> n) -> do
      push $ TakeResources iid (2 + n) (toAbilitySource attrs 1) False
      pure a
    _ -> Burglary2 <$> runMessage msg attrs
