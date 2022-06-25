module Arkham.Act.Cards.TheKingInTatters
  ( TheKingInTatters(..)
  , theKingInTatters
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheKingInTatters = TheKingInTatters ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theKingInTatters :: ActCard TheKingInTatters
theKingInTatters = act (3, A) TheKingInTatters Cards.theKingInTatters Nothing

instance RunMessage TheKingInTatters where
  runMessage msg (TheKingInTatters attrs) = TheKingInTatters <$> runMessage msg attrs
