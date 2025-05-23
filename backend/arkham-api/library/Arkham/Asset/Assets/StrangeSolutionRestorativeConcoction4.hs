module Arkham.Asset.Assets.StrangeSolutionRestorativeConcoction4 (
  strangeSolutionRestorativeConcoction4,
) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StrangeSolutionRestorativeConcoction4 = StrangeSolutionRestorativeConcoction4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolutionRestorativeConcoction4
  :: AssetCard StrangeSolutionRestorativeConcoction4
strangeSolutionRestorativeConcoction4 =
  asset StrangeSolutionRestorativeConcoction4 Cards.strangeSolutionRestorativeConcoction4

instance HasAbilities StrangeSolutionRestorativeConcoction4 where
  getAbilities (StrangeSolutionRestorativeConcoction4 x) =
    [ controlled x 1 (exists $ HealableInvestigator (toSource x) #damage $ colocatedWithMatch You)
        $ actionAbilityWithCost (assetUseCost x Supply 1)
    ]

instance RunMessage StrangeSolutionRestorativeConcoction4 where
  runMessage msg a@(StrangeSolutionRestorativeConcoction4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (toSource attrs) #damage $ colocatedWith iid
      chooseTargetM iid investigators \x -> healDamage x attrs 2
      pure a
    _ -> StrangeSolutionRestorativeConcoction4 <$> liftRunMessage msg attrs
