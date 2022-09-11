module Arkham.Act.Cards.StrangeOccurences
  ( StrangeOccurences(..)
  , strangeOccurences
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype StrangeOccurences = StrangeOccurences ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

strangeOccurences :: ActCard StrangeOccurences
strangeOccurences =
  act (3, E) StrangeOccurences Cards.strangeOccurences Nothing

instance RunMessage StrangeOccurences where
  runMessage msg (StrangeOccurences attrs) =
    StrangeOccurences <$> runMessage msg attrs
