module Arkham.Asset.Assets.CrystalRemainsTheChild (crystalRemainsTheChild) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetDefeated)
import Arkham.Card (setFacedown, toCard)
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Lair))

newtype CrystalRemainsTheChild = CrystalRemainsTheChild AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalRemainsTheChild :: AssetCard CrystalRemainsTheChild
crystalRemainsTheChild = assetWith CrystalRemainsTheChild Cards.crystalRemainsTheChild $ (healthL ?~ 1) . (sanityL ?~ 1)

instance HasAbilities CrystalRemainsTheChild where
  getAbilities (CrystalRemainsTheChild a) =
    [ controlled_ a 1 $ forced (TurnBegins #when You)
    , mkAbility a 2 $ forced $ AssetDefeated #when ByAny (be a)
    ]

instance RunMessage CrystalRemainsTheChild where
  runMessage msg a@(CrystalRemainsTheChild attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
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
    _ -> CrystalRemainsTheChild <$> liftRunMessage msg attrs
