module Arkham.Asset.Assets.CrystalRemainsTheMother (crystalRemainsTheMother) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card (setFacedown, toCard)
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Lair))

newtype CrystalRemainsTheMother = CrystalRemainsTheMother AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalRemainsTheMother :: AssetCard CrystalRemainsTheMother
crystalRemainsTheMother = assetWith CrystalRemainsTheMother Cards.crystalRemainsTheMother (healthL ?~ 1)

instance HasAbilities CrystalRemainsTheMother where
  getAbilities (CrystalRemainsTheMother a) =
    [ controlled_ a 1 $ forced (TurnBegins #when You)
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage CrystalRemainsTheMother where
  runMessage msg a@(CrystalRemainsTheMother attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws _ -> do
      cancelWindowBatch ws
      topTwo <- take 2 . unDeck <$> getEncounterDeck
      for_ topTwo obtainCard
      shuffled <- shuffle $ toCard attrs : map toCard topTwo
      facedown <- traverse (setFacedown True) shuffled
      lairs <- select $ NearestLocationTo iid (LocationWithTrait Lair)
      chooseOrRunOneM iid $ targets lairs $ \lid -> placeUnderneath lid facedown
      pure a
    _ -> CrystalRemainsTheMother <$> liftRunMessage msg attrs
