module Arkham.Homebrew.CircusExMortis.Locations.ForestPassage (forestPassage) where

import Arkham.Ability
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ForestPassage = ForestPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forestPassage :: LocationCard ForestPassage
forestPassage = location ForestPassage Cards.forestPassage 2 (Static 1)

instance HasAbilities ForestPassage where
  getAbilities (ForestPassage a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> exists (LocationWithTitle "Moonlit Forest" <> #unrevealed))
      $ FastAbility (PlaceClueOnLocationCost (PerPlayer 1))

instance RunMessage ForestPassage where
  runMessage msg l@(ForestPassage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      forests <- select $ LocationWithTitle "Moonlit Forest" <> #unrevealed
      chooseTargetM iid forests $ lookAtRevealed iid (attrs.ability 1)
      pure l
    _ -> ForestPassage <$> liftRunMessage msg attrs
