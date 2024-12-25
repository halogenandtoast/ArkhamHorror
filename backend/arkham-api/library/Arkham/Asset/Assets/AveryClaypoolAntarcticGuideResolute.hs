module Arkham.Asset.Assets.AveryClaypoolAntarcticGuideResolute (
  averyClaypoolAntarcticGuideResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype AveryClaypoolAntarcticGuideResolute = AveryClaypoolAntarcticGuideResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

averyClaypoolAntarcticGuideResolute :: AssetCard AveryClaypoolAntarcticGuideResolute
averyClaypoolAntarcticGuideResolute =
  allyWith
    AveryClaypoolAntarcticGuideResolute
    Cards.averyClaypoolAntarcticGuideResolute
    (4, 3)
    noSlots

instance HasAbilities AveryClaypoolAntarcticGuideResolute where
  getAbilities (AveryClaypoolAntarcticGuideResolute a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken #when (affectsOthers $ InvestigatorAt YourLocation) #frost)
          (exhaust a)
    ]

instance RunMessage AveryClaypoolAntarcticGuideResolute where
  runMessage msg a@(AveryClaypoolAntarcticGuideResolute attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      cancelChaosToken (attrs.ability 1) token
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      chooseOneM iid do
        labeled "Reveal a new chaos token" do
          getSkillTestInvestigator >>= traverse_ drawAnotherChaosToken
        labeled "Spend 1 Supply" $ spendUses (attrs.ability 1) attrs Supply 1
      pure a
    _ -> AveryClaypoolAntarcticGuideResolute <$> liftRunMessage msg attrs
