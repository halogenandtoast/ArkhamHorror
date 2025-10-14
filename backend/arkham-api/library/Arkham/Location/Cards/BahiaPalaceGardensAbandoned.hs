module Arkham.Location.Cards.BahiaPalaceGardensAbandoned (bahiaPalaceGardensAbandoned) where

import Arkham.Ability
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BahiaPalaceGardensAbandoned = BahiaPalaceGardensAbandoned LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bahiaPalaceGardensAbandoned :: LocationCard BahiaPalaceGardensAbandoned
bahiaPalaceGardensAbandoned = symbolLabel $ location BahiaPalaceGardensAbandoned Cards.bahiaPalaceGardensAbandoned 4 (Static 0)

instance HasAbilities BahiaPalaceGardensAbandoned where
  getAbilities (BahiaPalaceGardensAbandoned a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ TurnEnds #when You

instance RunMessage BahiaPalaceGardensAbandoned where
  runMessage msg l@(BahiaPalaceGardensAbandoned attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
        countVar 1 $ labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
      pure l
    _ -> BahiaPalaceGardensAbandoned <$> liftRunMessage msg attrs
