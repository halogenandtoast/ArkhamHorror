module Arkham.Homebrew.CircusExMortis.Locations.Flatcar (flatcar) where

import Arkham.Ability
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Flatcar = Flatcar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flatcar :: LocationCard Flatcar
flatcar = location Flatcar Cards.flatcar 1 (Static 1)

instance HasAbilities Flatcar where
  getAbilities (Flatcar a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> thisExists a LocationWithAnyClues)
      $ forced (TurnEnds #after You)

instance RunMessage Flatcar where
  runMessage msg l@(Flatcar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAndDiscardCard iid (attrs.ability 1)
      pure l
    _ -> Flatcar <$> liftRunMessage msg attrs
