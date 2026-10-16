module Arkham.Homebrew.TheLegendOfSleepyHollow.Assets.VanBruntsReward (vanBruntsReward) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted hiding (PlayCard)
import Arkham.Helpers.Window (cardPlayed)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype VanBruntsReward = VanBruntsReward AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vanBruntsReward :: AssetCard VanBruntsReward
vanBruntsReward = asset VanBruntsReward Cards.vanBruntsReward

instance HasAbilities VanBruntsReward where
  getAbilities (VanBruntsReward a) =
    [ controlled a 1 NoRestriction
        $ triggered (PlayCard #when You (basic $ CardWithCostAtLeast 4)) (exhaust a)
    ]

instance RunMessage VanBruntsReward where
  runMessage msg a@(VanBruntsReward attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (cardPlayed -> card) _ -> do
      reduceCostOf (attrs.ability 1) card 1
      pure a
    _ -> VanBruntsReward <$> liftRunMessage msg attrs
