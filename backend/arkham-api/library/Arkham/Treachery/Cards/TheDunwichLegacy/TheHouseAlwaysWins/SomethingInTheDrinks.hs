module Arkham.Treachery.Cards.TheDunwichLegacy.TheHouseAlwaysWins.SomethingInTheDrinks (somethingInTheDrinks) where

import Arkham.Scenarios.TheDunwichLegacy.TheHouseAlwaysWins.Helpers
import Arkham.Treachery.CardDefs.TheDunwichLegacy.TheHouseAlwaysWins qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype SomethingInTheDrinks = SomethingInTheDrinks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingInTheDrinks :: TreacheryCard SomethingInTheDrinks
somethingInTheDrinks = treachery SomethingInTheDrinks Cards.somethingInTheDrinks

instance RunMessage SomethingInTheDrinks where
  runMessage msg t@(SomethingInTheDrinks attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      each_ hadDrinks \drinker -> loseActions drinker attrs 1
      pure t
    _ -> SomethingInTheDrinks <$> liftRunMessage msg attrs
