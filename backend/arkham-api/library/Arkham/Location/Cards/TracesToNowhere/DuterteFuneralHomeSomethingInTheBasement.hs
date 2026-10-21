module Arkham.Location.Cards.TracesToNowhere.DuterteFuneralHomeSomethingInTheBasement (
  duterteFuneralHomeSomethingInTheBasement,
) where

import Arkham.Ability
import Arkham.Helpers.Story
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Story.CardDefs.TracesToNowhere qualified as Stories

newtype DuterteFuneralHomeSomethingInTheBasement = DuterteFuneralHomeSomethingInTheBasement LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

duterteFuneralHomeSomethingInTheBasement :: LocationCard DuterteFuneralHomeSomethingInTheBasement
duterteFuneralHomeSomethingInTheBasement =
  location
    DuterteFuneralHomeSomethingInTheBasement
    Cards.duterteFuneralHomeSomethingInTheBasement
    1
    (PerPlayer 1)

instance HasAbilities DuterteFuneralHomeSomethingInTheBasement where
  getAbilities (DuterteFuneralHomeSomethingInTheBasement a) =
    extendRevealed
      a
      [restricted a 1 (Here <> thisIs a LocationWithoutClues) actionAbility, hireACab a 2]

instance RunMessage DuterteFuneralHomeSomethingInTheBasement where
  runMessage msg l@(DuterteFuneralHomeSomethingInTheBasement attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs Stories.pulledBelow
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      hireACabEffect iid (attrs.ability 2)
      pure l
    _ -> DuterteFuneralHomeSomethingInTheBasement <$> liftRunMessage msg attrs
