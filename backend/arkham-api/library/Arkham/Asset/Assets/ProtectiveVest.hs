module Arkham.Asset.Assets.ProtectiveVest (protectiveVest) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy
import Arkham.Trait (Trait (Upgrade))

newtype ProtectiveVest = ProtectiveVest AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectiveVest :: AssetCard ProtectiveVest
protectiveVest = assetWith ProtectiveVest Cards.protectiveVest (healthL ?~ 2)

instance HasAbilities ProtectiveVest where
  getAbilities (ProtectiveVest a) =
    [controlled_ a 1 $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage ProtectiveVest where
  runMessage msg a@(ProtectiveVest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let matcher = basic $ oneOf [#firearm, CardWithTrait Upgrade]
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] matcher (DrawFound iid 1)
      pure a
    _ -> ProtectiveVest <$> liftRunMessage msg attrs
