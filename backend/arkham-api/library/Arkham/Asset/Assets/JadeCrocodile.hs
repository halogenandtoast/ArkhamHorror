module Arkham.Asset.Assets.JadeCrocodile (jadeCrocodile) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.RelicsOfThePast.Helpers

newtype JadeCrocodile = JadeCrocodile AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jadeCrocodile :: AssetCard JadeCrocodile
jadeCrocodile = asset JadeCrocodile Cards.jadeCrocodile

instance HasAbilities JadeCrocodile where
  getAbilities (JadeCrocodile a) =
    [restricted a 1 ControlsThis $ forced $ InvestigatorResigned #when You]

instance RunMessage JadeCrocodile where
  runMessage msg a@(JadeCrocodile attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      chooseOneM iid $ scenarioI18n do
        labeled' "placeDoomOnYourLocation" $ withLocationOf iid \lid -> placeDoom attrs lid 1
        unscoped $ countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure a
    _ -> JadeCrocodile <$> liftRunMessage msg attrs
