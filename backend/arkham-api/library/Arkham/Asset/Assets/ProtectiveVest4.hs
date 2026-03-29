module Arkham.Asset.Assets.ProtectiveVest4 (protectiveVest4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Slot
import Arkham.Strategy
import Arkham.Trait (Trait (Firearm, Upgrade))

newtype ProtectiveVest4 = ProtectiveVest4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

protectiveVest4 :: AssetCard ProtectiveVest4
protectiveVest4 = assetWith ProtectiveVest4 Cards.protectiveVest4 (healthL ?~ 3)

instance HasAbilities ProtectiveVest4 where
  getAbilities (ProtectiveVest4 a) =
    [controlled_ a 1 $ freeReaction $ AssetEntersPlay #after (be a)]

instance RunMessage ProtectiveVest4 where
  runMessage msg a@(ProtectiveVest4 attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | card.id == attrs.cardId -> do
      push $ AddSlot iid #hand $ RestrictedSlot (toSource attrs) (CardWithTrait Firearm) []
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let matcher = basic $ oneOf [#firearm, CardWithTrait Upgrade]
      search iid (attrs.ability 1) iid [fromTopOfDeck 9] matcher (DrawFound iid 1)
      pure a
    _ -> ProtectiveVest4 <$> liftRunMessage msg attrs
