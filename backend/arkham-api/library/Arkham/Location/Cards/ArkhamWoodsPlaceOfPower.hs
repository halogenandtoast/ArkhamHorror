module Arkham.Location.Cards.ArkhamWoodsPlaceOfPower (arkhamWoodsPlaceOfPower) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ArkhamWoodsPlaceOfPower = ArkhamWoodsPlaceOfPower LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

arkhamWoodsPlaceOfPower :: LocationCard ArkhamWoodsPlaceOfPower
arkhamWoodsPlaceOfPower = location ArkhamWoodsPlaceOfPower Cards.arkhamWoodsPlaceOfPower 3 (PerPlayer 1)

instance HasModifiersFor ArkhamWoodsPlaceOfPower where
  getModifiersFor (ArkhamWoodsPlaceOfPower a) = do
    modifySelect
      a
      (InvestigatorAt (be a))
      [CannotCancelCardOrGameEffects, CannotIgnoreCardOrGameEffects, DrawGainsPeril]

instance RunMessage ArkhamWoodsPlaceOfPower where
  runMessage msg (ArkhamWoodsPlaceOfPower attrs) = ArkhamWoodsPlaceOfPower <$> runMessage msg attrs
