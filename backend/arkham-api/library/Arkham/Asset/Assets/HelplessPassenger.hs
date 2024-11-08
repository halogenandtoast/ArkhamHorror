module Arkham.Asset.Assets.HelplessPassenger (helplessPassenger, HelplessPassenger (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Direction
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Matcher
import Arkham.Message.Lifted.Placement

newtype HelplessPassenger = HelplessPassenger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

helplessPassenger :: AssetCard HelplessPassenger
helplessPassenger =
  allyWith HelplessPassenger Cards.helplessPassenger (1, 1)
    $ (isStoryL .~ True)
    . (slotsL .~ mempty)

instance HasAbilities HelplessPassenger where
  getAbilities (HelplessPassenger x) =
    [ restrictedAbility x 1 (Uncontrolled <> OnSameLocation) parleyAction_
    , mkAbility x 2 $ forced $ AssetLeavesPlay #when (be x)
    ]

instance RunMessage HelplessPassenger where
  runMessage msg a@(HelplessPassenger attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid \lid -> do
        place attrs =<< selectOrDefault lid (LocationInDirection LeftOf $ LocationWithId lid)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeControlOfAsset iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      eachInvestigator \iid -> assignHorror iid (attrs.ability 2) 1
      pure a
    _ -> HelplessPassenger <$> liftRunMessage msg attrs
