module Arkham.Location.Cards.ChapelOfStAubertThePathIsOpen
  ( chapelOfStAubertThePathIsOpen
  , ChapelOfStAubertThePathIsOpen(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types ( Field (InvestigatorRemainingSanity) )
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Projection
import Arkham.ScenarioLogKey
import Arkham.Target

newtype ChapelOfStAubertThePathIsOpen = ChapelOfStAubertThePathIsOpen LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chapelOfStAubertThePathIsOpen :: LocationCard ChapelOfStAubertThePathIsOpen
chapelOfStAubertThePathIsOpen = location
  ChapelOfStAubertThePathIsOpen
  Cards.chapelOfStAubertThePathIsOpen
  3
  (PerPlayer 2)

instance HasModifiersFor ChapelOfStAubertThePathIsOpen where
  getModifiersFor target (ChapelOfStAubertThePathIsOpen attrs)
    | isTarget attrs target = do
      foundAGuide <- remembered FoundAGuide
      pure $ toModifiers
        attrs
        [ Blocked | not (locationRevealed attrs) && not foundAGuide ]
  getModifiersFor (InvestigatorTarget iid) (ChapelOfStAubertThePathIsOpen attrs)
    | iid `member` locationInvestigators attrs
    = do
      remainingSanity <- field InvestigatorRemainingSanity iid
      pure $ toModifiers attrs [ CannotDiscoverClues | remainingSanity > 3 ]
  getModifiersFor _ _ = pure []

instance HasAbilities ChapelOfStAubertThePathIsOpen where
  getAbilities (ChapelOfStAubertThePathIsOpen a) = withBaseAbilities
    a
    [restrictedAbility a 1 Here $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage ChapelOfStAubertThePathIsOpen where
  runMessage msg l@(ChapelOfStAubertThePathIsOpen attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ chooseAmounts
        iid
        "Take up to 3 horror"
        3
        [("Horror", (0, 3))]
        (toTarget attrs)
      pure l
    ResolveAmounts iid choices target | isTarget attrs target -> do
      let
        choicesMap = mapFromList @(HashMap Text Int) choices
        horrorAmount = findWithDefault 0 "Horror" choicesMap
      when (horrorAmount > 0) $ push $ InvestigatorAssignDamage
        iid
        (toSource attrs)
        DamageAny
        0
        horrorAmount
      pure l
    _ -> ChapelOfStAubertThePathIsOpen <$> runMessage msg attrs
