module Arkham.Homebrew.CircusExMortis.Locations.SparseWoodland (sparseWoodland) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelectMapM)
import Arkham.Homebrew.CircusExMortis.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (getSealedMoonTokens)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SparseWoodland = SparseWoodland LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sparseWoodland :: LocationCard SparseWoodland
sparseWoodland = location SparseWoodland Cards.sparseWoodland 3 (PerPlayer 1)

-- "Each investigator at Sparse Woodland gets -2 maximum hand size for each ☾ token
-- sealed on their investigator card." X is per investigator, so the modifier is mapped
-- over the selection rather than shared.
instance HasModifiersFor SparseWoodland where
  getModifiersFor (SparseWoodland a) = whenRevealed a do
    modifySelectMapM a (investigatorAt a) \iid -> do
      n <- length <$> getSealedMoonTokens iid
      pure [HandSize (-(2 * n)) | n > 0]

instance RunMessage SparseWoodland where
  runMessage msg (SparseWoodland attrs) = SparseWoodland <$> runMessage msg attrs
