module Arkham.Act.Cards.TheBrotherhoodIsRevealed
  ( TheBrotherhoodIsRevealed(..)
  , theBrotherhoodIsRevealed
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheBrotherhoodIsRevealed = TheBrotherhoodIsRevealed ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theBrotherhoodIsRevealed :: ActCard TheBrotherhoodIsRevealed
theBrotherhoodIsRevealed =
  act (3, E) TheBrotherhoodIsRevealed Cards.theBrotherhoodIsRevealed Nothing

instance RunMessage TheBrotherhoodIsRevealed where
  runMessage msg (TheBrotherhoodIsRevealed attrs) =
    TheBrotherhoodIsRevealed <$> runMessage msg attrs
