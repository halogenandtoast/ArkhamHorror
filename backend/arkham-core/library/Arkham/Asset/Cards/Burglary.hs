module Arkham.Asset.Cards.Burglary (
  Burglary (..),
  burglary,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate

newtype Burglary = Burglary AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

burglary :: AssetCard Burglary
burglary = asset Burglary Cards.burglary

instance HasAbilities Burglary where
  getAbilities (Burglary a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Burglary where
  runMessage msg a@(Burglary attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushM $ mkInvestigate iid (toAbilitySource attrs 1) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      push $ TakeResources iid 3 (toAbilitySource attrs 1) False
      pure a
    _ -> Burglary <$> runMessage msg attrs
