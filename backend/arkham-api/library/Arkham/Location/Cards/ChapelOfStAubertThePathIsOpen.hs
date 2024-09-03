module Arkham.Location.Cards.ChapelOfStAubertThePathIsOpen (
  chapelOfStAubertThePathIsOpen,
  ChapelOfStAubertThePathIsOpen (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (InvestigatorRemainingSanity))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype ChapelOfStAubertThePathIsOpen = ChapelOfStAubertThePathIsOpen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelOfStAubertThePathIsOpen :: LocationCard ChapelOfStAubertThePathIsOpen
chapelOfStAubertThePathIsOpen =
  location ChapelOfStAubertThePathIsOpen Cards.chapelOfStAubertThePathIsOpen 3 (PerPlayer 2)

instance HasModifiersFor ChapelOfStAubertThePathIsOpen where
  getModifiersFor target (ChapelOfStAubertThePathIsOpen attrs) | isTarget attrs target = do
    foundAGuide <- remembered FoundAGuide
    pure $ toModifiers attrs [Blocked | not (locationRevealed attrs) && not foundAGuide]
  getModifiersFor (InvestigatorTarget iid) (ChapelOfStAubertThePathIsOpen attrs) = do
    here <- iid `isAt` attrs
    remainingSanity <- field InvestigatorRemainingSanity iid
    pure $ toModifiers attrs [CannotDiscoverClues | here, remainingSanity > 3]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapelOfStAubertThePathIsOpen where
  getAbilities (ChapelOfStAubertThePathIsOpen a) =
    withBaseAbilities a [restrictedAbility a 1 Here actionAbility]

instance RunMessage ChapelOfStAubertThePathIsOpen where
  runMessage msg l@(ChapelOfStAubertThePathIsOpen attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      pushM $ chooseAmounts player "Take up to 3 horror" (MaxAmountTarget 3) [("Horror", (0, 3))] attrs
      pure l
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let horrorAmount = getChoiceAmount "Horror" choices
      pushWhen (horrorAmount > 0) $ assignHorror iid attrs horrorAmount
      pure l
    _ -> ChapelOfStAubertThePathIsOpen <$> runMessage msg attrs
