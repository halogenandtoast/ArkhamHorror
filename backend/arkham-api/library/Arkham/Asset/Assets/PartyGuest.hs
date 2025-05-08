module Arkham.Asset.Assets.PartyGuest (partyGuest) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Trait

newtype PartyGuest = PartyGuest AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

partyGuest :: AssetCard PartyGuest
partyGuest = asset PartyGuest Cards.partyGuest

instance HasModifiersFor PartyGuest where
  getModifiersFor (PartyGuest a) =
    modifySelect
      a
      Anyone
      [CannotTakeAction $ AssetAction #parley $ AssetWithTrait Bystander <> at_ (locationWithAsset a)]

instance HasAbilities PartyGuest where
  getAbilities (PartyGuest a) = [restricted a 1 OnSameLocation $ parleyAction (ResourceCost 2)]

instance RunMessage PartyGuest where
  runMessage msg a@(PartyGuest attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- select $ AssetWithMostClues $ AssetWithTrait Bystander
      chooseTargetM iid assets (`withLocationOf` place attrs)
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ ConnectedTo $ locationWithInvestigator iid
      chooseTargetM iid locations (place attrs)
      pure a
    _ -> PartyGuest <$> liftRunMessage msg attrs
