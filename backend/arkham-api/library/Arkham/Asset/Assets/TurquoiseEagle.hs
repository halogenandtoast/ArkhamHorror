module Arkham.Asset.Assets.TurquoiseEagle (turquoiseEagle) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (InvestigatorResigned)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.RelicsOfThePast.Helpers

newtype TurquoiseEagle = TurquoiseEagle AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

turquoiseEagle :: AssetCard TurquoiseEagle
turquoiseEagle = asset TurquoiseEagle Cards.turquoiseEagle

instance HasAbilities TurquoiseEagle where
  getAbilities (TurquoiseEagle a) =
    [restricted a 1 ControlsThis $ forced $ InvestigatorResigned #when You]

instance RunMessage TurquoiseEagle where
  runMessage msg a@(TurquoiseEagle attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      chooseOneM iid $ scenarioI18n do
        labeled' "placeDoomOnYourLocation" $ withLocationOf iid \lid -> placeDoom attrs lid 1
        unscoped $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      addToVictory iid attrs
      pure a
    _ -> TurquoiseEagle <$> liftRunMessage msg attrs
