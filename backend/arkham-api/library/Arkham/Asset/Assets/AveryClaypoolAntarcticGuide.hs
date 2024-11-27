module Arkham.Asset.Assets.AveryClaypoolAntarcticGuide (
  averyClaypoolAntarcticGuide,
  AveryClaypoolAntarcticGuide (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (RevealChaosToken)
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Helpers.Window (getChaosToken)
import Arkham.Matcher

newtype AveryClaypoolAntarcticGuide = AveryClaypoolAntarcticGuide AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

averyClaypoolAntarcticGuide :: AssetCard AveryClaypoolAntarcticGuide
averyClaypoolAntarcticGuide = allyWith AveryClaypoolAntarcticGuide Cards.averyClaypoolAntarcticGuide (3, 3) noSlots

instance HasAbilities AveryClaypoolAntarcticGuide where
  getAbilities (AveryClaypoolAntarcticGuide a) =
    [ restricted a 1 ControlsThis
        $ ReactionAbility
          (RevealChaosToken #when (affectsOthers $ InvestigatorAt YourLocation) #frost)
          (exhaust a <> assetUseCost a Supply 1)
    ]

instance RunMessage AveryClaypoolAntarcticGuide where
  runMessage msg a@(AveryClaypoolAntarcticGuide attrs) = runQueueT $ case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 (getChaosToken -> token) _ -> do
      cancelChaosToken (attrs.ability 1) token
      cancelledOrIgnoredCardOrGameEffect (attrs.ability 1)
      getSkillTestInvestigator >>= traverse_ drawAnotherChaosToken
      pure a
    _ -> AveryClaypoolAntarcticGuide <$> liftRunMessage msg attrs
