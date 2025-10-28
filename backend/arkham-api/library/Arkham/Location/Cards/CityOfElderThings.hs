module Arkham.Location.Cards.CityOfElderThings (cityOfElderThings) where

import Arkham.Ability
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Scenario.Deck

newtype CityOfElderThings = CityOfElderThings LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cityOfElderThings :: LocationCard CityOfElderThings
cityOfElderThings = location CityOfElderThings Cards.cityOfElderThings 3 (PerPlayer 2)

instance HasAbilities CityOfElderThings where
  getAbilities (CityOfElderThings a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ triggered (RevealLocation #after You $ be a) (HorrorCost (toSource a) YouTarget 2)

instance RunMessage CityOfElderThings where
  runMessage msg l@(CityOfElderThings attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ lid | lid == toId attrs -> do
      CityOfElderThings <$> liftRunMessage msg (attrs & labelL .~ "cityOfElderThings")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DrawCards iid $ targetCardDraw attrs UnknownPlacesDeck 1
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      for_ drewCards.cards $ placeLabeledLocation "unknownPlaces" >=> connectBothWays attrs.id
      pure l
    _ -> CityOfElderThings <$> liftRunMessage msg attrs
