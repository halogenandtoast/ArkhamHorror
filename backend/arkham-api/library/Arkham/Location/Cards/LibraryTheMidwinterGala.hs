module Arkham.Location.Cards.LibraryTheMidwinterGala (libraryTheMidwinterGala) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator, isParley)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype LibraryTheMidwinterGala = LibraryTheMidwinterGala LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'LibraryTheMidwinterGala' from The Midwinter Gala (#71013).
libraryTheMidwinterGala :: LocationCard LibraryTheMidwinterGala
libraryTheMidwinterGala = location LibraryTheMidwinterGala Cards.libraryTheMidwinterGala 4 (PerPlayer 3)

instance HasModifiersFor LibraryTheMidwinterGala where
  getModifiersFor (LibraryTheMidwinterGala a) = do
    getSkillTestInvestigator >>= traverse_ \iid -> do
      maybeModified_ a iid do
        guardM isParley
        pure [AnySkillValue 1]

instance HasAbilities LibraryTheMidwinterGala where
  getAbilities (LibraryTheMidwinterGala a) =
    extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here $ FastAbility Free

instance RunMessage LibraryTheMidwinterGala where
  runMessage msg l@(LibraryTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- TODO: Implement Jewel interaction ability
      pure l
    _ -> LibraryTheMidwinterGala <$> liftRunMessage msg attrs
