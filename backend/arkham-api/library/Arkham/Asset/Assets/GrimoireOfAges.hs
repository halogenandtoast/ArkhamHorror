module Arkham.Asset.Assets.GrimoireOfAges (grimoireOfAges) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Dreaming.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose

newtype GrimoireOfAges = GrimoireOfAges AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grimoireOfAges :: AssetCard GrimoireOfAges
grimoireOfAges = asset GrimoireOfAges Cards.grimoireOfAges

instance HasAbilities GrimoireOfAges where
  getAbilities (GrimoireOfAges a) =
    [ restricted a 1 (ControlsThis <> CanMoveADream You) $ triggered_ (AssetEntersPlay #after (be a))
    , restricted a 2 (ControlsThis <> DuringTurn You)
        $ FastAbility (assetUseCost a Secret 1 <> exhaust a)
    ]

instance RunMessage GrimoireOfAges where
  runMessage msg a@(GrimoireOfAges attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveDreamBetweenDreamingCards iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select $ affectsColocated iid
      chooseOneM iid $ targets investigators \i -> drawCards i (attrs.ability 2) 1
      pure a
    _ -> GrimoireOfAges <$> liftRunMessage msg attrs
