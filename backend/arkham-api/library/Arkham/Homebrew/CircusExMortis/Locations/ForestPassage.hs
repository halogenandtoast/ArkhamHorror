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
forestPassage =
  locationWith ForestPassage Cards.forestPassage 2 (Static 1) connectsToAdjacent

instance HasAbilities ForestPassage where
  getAbilities (ForestPassage a) =
    -- [fast] Place 1 [perPlayer] of your clues on Forest Passage: Look at the
    -- revealed side of any copy of Moonlit Forest.
    -- TODO(homebrew): PlaceClueOnLocationCost places the clues on the acting
    -- investigator's current location rather than always Forest Passage; this is
    -- correct whenever the ability is used while at Forest Passage (the expected case).
    extendRevealed1 a
      $ mkAbility a 1
      $ FastAbility (PlaceClueOnLocationCost (PerPlayer 1))

instance RunMessage ForestPassage where
  runMessage msg l@(ForestPassage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      forests <- select $ LocationWithTitle "Moonlit Forest"
      chooseTargetM iid forests \lid -> lookAtRevealed iid (attrs.ability 1) lid
      pure l
    _ -> ForestPassage <$> liftRunMessage msg attrs
