module Arkham.Location.Cards.RingLibraryArchiveOfTheStars (ringLibraryArchiveOfTheStars) where

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted hiding (Discarded)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Passageway))

newtype RingLibraryArchiveOfTheStars = RingLibraryArchiveOfTheStars LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ringLibraryArchiveOfTheStars :: LocationCard RingLibraryArchiveOfTheStars
ringLibraryArchiveOfTheStars = location RingLibraryArchiveOfTheStars Cards.ringLibraryArchiveOfTheStars 2 (Static 2)

instance HasAbilities RingLibraryArchiveOfTheStars where
  getAbilities (RingLibraryArchiveOfTheStars a) =
    extendRevealed
      a
      [ reaction
          a
          1
          NoRestriction
          (HandDiscardCost 1 #any)
          (Discarded #when (Just You) AnySource (basic IsEncounterCard))
      , restricted a 2 (Here <> exists (not_ (be a) <> RevealedLocation <> LocationWithTrait Passageway)) actionAbility
      ]

instance RunMessage RingLibraryArchiveOfTheStars where
  runMessage msg l@(RingLibraryArchiveOfTheStars attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfEncounterDeck iid (attrs.ability 1) 2
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      passageways <- select $ not_ (be attrs) <> RevealedLocation <> LocationWithTrait Passageway
      chooseTargetM iid passageways $ moveTo (attrs.ability 2) iid
      pure l
    _ -> RingLibraryArchiveOfTheStars <$> liftRunMessage msg attrs
