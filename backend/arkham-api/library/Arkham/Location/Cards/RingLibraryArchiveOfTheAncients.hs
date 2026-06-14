module Arkham.Location.Cards.RingLibraryArchiveOfTheAncients (ringLibraryArchiveOfTheAncients) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (Discarded)
import Arkham.Matcher

newtype RingLibraryArchiveOfTheAncients = RingLibraryArchiveOfTheAncients LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ringLibraryArchiveOfTheAncients :: LocationCard RingLibraryArchiveOfTheAncients
ringLibraryArchiveOfTheAncients = location RingLibraryArchiveOfTheAncients Cards.ringLibraryArchiveOfTheAncients 3 (Static 1)

instance HasAbilities RingLibraryArchiveOfTheAncients where
  getAbilities (RingLibraryArchiveOfTheAncients a) =
    extendRevealed1 a
      $ reaction
        a
        1
        NoRestriction
        (DirectHorrorCost (toSource a) You 1)
        (Discarded #when (Just You) AnySource (basic IsEncounterCard))

instance RunMessage RingLibraryArchiveOfTheAncients where
  runMessage msg l@(RingLibraryArchiveOfTheAncients attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeck iid (attrs.ability 1) 3
      pure l
    _ -> RingLibraryArchiveOfTheAncients <$> liftRunMessage msg attrs
