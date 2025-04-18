module Arkham.Asset.Assets.DanielChesterfield (danielChesterfield) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype DanielChesterfield = DanielChesterfield AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

danielChesterfield :: AssetCard DanielChesterfield
danielChesterfield = ally DanielChesterfield Cards.danielChesterfield (1, 3)

instance HasAbilities DanielChesterfield where
  getAbilities (DanielChesterfield a) =
    [ controlled a 1 (exists (NotYou <> at_ YourLocation)) $ FastAbility Free
    , restricted a 2 ControlsThis
        $ forced
        $ AssignedHorror #after You (ExcludesTarget $ TargetIs $ toTarget a)
    , mkAbility a 3 $ forced $ AssetLeavesPlay #when (be a)
    ]

instance RunMessage DanielChesterfield where
  runMessage msg a@(DanielChesterfield attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      others <- select $ at_ YourLocation <> not_ (InvestigatorWithId iid)
      chooseTargetM iid others (`takeControlOfAsset` attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assignDamage iid (attrs.ability 2) 1
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      removeFromGame attrs
      pure a
    _ -> DanielChesterfield <$> liftRunMessage msg attrs
