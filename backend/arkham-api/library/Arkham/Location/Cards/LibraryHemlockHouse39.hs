module Arkham.Location.Cards.LibraryHemlockHouse39 (libraryHemlockHouse39) where

import Arkham.Ability
import Arkham.Classes.HasGame
import Arkham.Helpers.Location (getLocationGlobalMeta)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log (remember)
import Arkham.ScenarioLogKey (ScenarioLogKey (LibrarySecretPassageOpened))
import Arkham.Token (Token (..))
import Arkham.Tracing

newtype LibraryHemlockHouse39 = LibraryHemlockHouse39 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

libraryHemlockHouse39 :: LocationCard LibraryHemlockHouse39
libraryHemlockHouse39 =
  locationWith LibraryHemlockHouse39 Cards.libraryHemlockHouse39 3 (PerPlayer 1) connectsToAdjacent

-- The chosen "secret passage" location id is recorded in the meta of whichever
-- Library triggered the reaction. Both copies of Library look up the marker
-- across all Library locations so the connection applies symmetrically.
findSecretPassage :: (HasGame m, Tracing m) => m (Maybe LocationId)
findSecretPassage = do
  libs <- select (LocationWithTitle "Library")
  asum <$> traverse (getLocationGlobalMeta @LocationId "secretPassage") libs

instance HasModifiersFor LibraryHemlockHouse39 where
  getModifiersFor (LibraryHemlockHouse39 a) = do
    findSecretPassage >>= traverse_ \chosen -> do
      modifySelf a [ConnectedToWhen (be a) (LocationWithId chosen)]
      modifySelect
        a
        (LocationWithId chosen)
        [ConnectedToWhen (LocationWithId chosen) (LocationWithTitle "Library")]

instance HasAbilities LibraryHemlockHouse39 where
  getAbilities (LibraryHemlockHouse39 a) =
    extendRevealed1 a
      $ restricted a 1 (not_ (Remembered LibrarySecretPassageOpened))
      $ freeReaction (DiscoveringLastClue #after You (be a))

instance RunMessage LibraryHemlockHouse39 where
  runMessage msg l@(LibraryHemlockHouse39 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select Anywhere
      chooseTargetM iid locations \chosen -> do
        placeTokens (attrs.ability 1) chosen Horror 1
        setGlobal attrs "secretPassage" chosen
        remember LibrarySecretPassageOpened
      pure l
    _ -> LibraryHemlockHouse39 <$> liftRunMessage msg attrs
