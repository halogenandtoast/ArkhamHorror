module Arkham.Act.Cards.StrangeRelicsMariaDeSilva
  ( StrangeRelicsMariaDeSilva(..)
  , strangeRelicsMariaDeSilva
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype StrangeRelicsMariaDeSilva = StrangeRelicsMariaDeSilva ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

strangeRelicsMariaDeSilva :: ActCard StrangeRelicsMariaDeSilva
strangeRelicsMariaDeSilva =
  act (2, E) StrangeRelicsMariaDeSilva Cards.strangeRelicsMariaDeSilva Nothing

instance RunMessage StrangeRelicsMariaDeSilva where
  runMessage msg (StrangeRelicsMariaDeSilva attrs) =
    StrangeRelicsMariaDeSilva <$> runMessage msg attrs
