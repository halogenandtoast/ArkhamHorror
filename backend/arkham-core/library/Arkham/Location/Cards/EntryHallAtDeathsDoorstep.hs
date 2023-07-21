module Arkham.Location.Cards.EntryHallAtDeathsDoorstep (
  entryHallAtDeathsDoorstep,
  EntryHallAtDeathsDoorstep (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype EntryHallAtDeathsDoorstep = EntryHallAtDeathsDoorstep LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

entryHallAtDeathsDoorstep :: LocationCard EntryHallAtDeathsDoorstep
entryHallAtDeathsDoorstep = location EntryHallAtDeathsDoorstep Cards.entryHallAtDeathsDoorstep 3 (Static 0)

instance HasAbilities EntryHallAtDeathsDoorstep where
  getAbilities (EntryHallAtDeathsDoorstep attrs) =
    withBaseAbilities
      attrs
      [ withTooltip
        "\"I guess there is nothing to those disappearances after all.\""
        (locationResignAction attrs)
      | locationRevealed attrs
      ]

instance RunMessage EntryHallAtDeathsDoorstep where
  runMessage msg (EntryHallAtDeathsDoorstep attrs) =
    EntryHallAtDeathsDoorstep <$> runMessage msg attrs
