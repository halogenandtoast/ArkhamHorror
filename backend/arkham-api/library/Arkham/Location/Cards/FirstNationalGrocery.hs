module Arkham.Location.Cards.FirstNationalGrocery (firstNationalGrocery, FirstNationalGrocery (..)) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Strategy

newtype FirstNationalGrocery = FirstNationalGrocery LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstNationalGrocery :: LocationCard FirstNationalGrocery
firstNationalGrocery = location FirstNationalGrocery Cards.firstNationalGrocery 3 (PerPlayer 1)

instance HasAbilities FirstNationalGrocery where
  getAbilities (FirstNationalGrocery a) =
    extendRevealed1 a $ restricted a 1 (Here <> can.search.deck You) actionAbility

instance RunMessage FirstNationalGrocery where
  runMessage msg l@(FirstNationalGrocery attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] (basic #item) (DrawFound iid 1)
      pure l
    _ -> FirstNationalGrocery <$> liftRunMessage msg attrs
