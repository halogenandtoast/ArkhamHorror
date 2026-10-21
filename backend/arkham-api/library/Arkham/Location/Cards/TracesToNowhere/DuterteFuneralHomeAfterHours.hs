module Arkham.Location.Cards.TracesToNowhere.DuterteFuneralHomeAfterHours (
  duterteFuneralHomeAfterHours,
) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype DuterteFuneralHomeAfterHours = DuterteFuneralHomeAfterHours LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duterteFuneralHomeAfterHours :: LocationCard DuterteFuneralHomeAfterHours
duterteFuneralHomeAfterHours =
  location DuterteFuneralHomeAfterHours Cards.duterteFuneralHomeAfterHours 3 (PerPlayer 1)

instance HasAbilities DuterteFuneralHomeAfterHours where
  getAbilities (DuterteFuneralHomeAfterHours a) =
    extendRevealed
      a
      [restricted a 1 (Here <> thisIs a LocationWithoutClues) actionAbility, hireACab a 2]

instance RunMessage DuterteFuneralHomeAfterHours where
  runMessage msg l@(DuterteFuneralHomeAfterHours attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.aGlimpseBeyond
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hireACabEffect iid (attrs.ability 2)
      pure l
    _ -> DuterteFuneralHomeAfterHours <$> liftRunMessage msg attrs
