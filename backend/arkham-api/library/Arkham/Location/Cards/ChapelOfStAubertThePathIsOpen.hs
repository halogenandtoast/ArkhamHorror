module Arkham.Location.Cards.ChapelOfStAubertThePathIsOpen (
  chapelOfStAubertThePathIsOpen,
  ChapelOfStAubertThePathIsOpen (..),
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message (getChoiceAmount)
import Arkham.ScenarioLogKey

newtype ChapelOfStAubertThePathIsOpen = ChapelOfStAubertThePathIsOpen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelOfStAubertThePathIsOpen :: LocationCard ChapelOfStAubertThePathIsOpen
chapelOfStAubertThePathIsOpen =
  location ChapelOfStAubertThePathIsOpen Cards.chapelOfStAubertThePathIsOpen 3 (PerPlayer 2)

instance HasModifiersFor ChapelOfStAubertThePathIsOpen where
  getModifiersFor (ChapelOfStAubertThePathIsOpen a) =
    if a.revealed
      then modifySelect a (InvestigatorWithRemainingSanity $ atMost 3) [CannotDiscoverCluesAt (be a)]
      else blockedUnless a $ remembered FoundAGuide

instance HasAbilities ChapelOfStAubertThePathIsOpen where
  getAbilities (ChapelOfStAubertThePathIsOpen a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage ChapelOfStAubertThePathIsOpen where
  runMessage msg l@(ChapelOfStAubertThePathIsOpen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAmounts iid "Take up to 3 horror" (MaxAmountTarget 3) [("Horror", (0, 3))] attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "Horror" -> horrorAmount) (isTarget attrs -> True) -> do
      when (horrorAmount > 0) $ assignHorror iid (attrs.ability 1) horrorAmount
      pure l
    _ -> ChapelOfStAubertThePathIsOpen <$> liftRunMessage msg attrs
