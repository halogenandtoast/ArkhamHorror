module Arkham.Location.Cards.RingLibraryArchiveOfTheAncients (ringLibraryArchiveOfTheAncients) where

import Arkham.Ability
import Arkham.Classes.HasQueue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (Discarded)
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

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
        (WouldDiscardTopOfEncounterDeck #when You AnySource)

instance RunMessage RingLibraryArchiveOfTheAncients where
  runMessage msg l@(RingLibraryArchiveOfTheAncients attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.WouldDiscardTopOfEncounterDeck iid source _ -> lift do
          replaceMessageMatching
            ( \case
                DiscardTopOfEncounterDeckWithDiscardedCards iid' _ source' _ [] ->
                  iid' == iid && source' == source
                _ -> False
            )
            ( \case
                DiscardTopOfEncounterDeckWithDiscardedCards iid' n source' mtarget cards ->
                  [DiscardTopOfEncounterDeckWithDiscardedCards iid' (n + 3) source' mtarget cards]
                other -> [other]
            )
        _ -> pure ()
      pure l
    _ -> RingLibraryArchiveOfTheAncients <$> liftRunMessage msg attrs
