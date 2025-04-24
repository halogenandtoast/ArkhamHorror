module Arkham.Asset.Assets.DrHenryArmitage (drHenryArmitage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Matcher

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

instance HasAbilities DrHenryArmitage where
  getAbilities (DrHenryArmitage a) =
    [ controlled a 1 (youExist can.gain.resources)
        $ triggered
          (DrawCard #after You (basic DiscardableCard) (DeckOf You))
          (DiscardDrawnCardCost <> exhaust a)
    ]

instance RunMessage DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 3
      pure a
    _ -> DrHenryArmitage <$> liftRunMessage msg attrs
