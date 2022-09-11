module Arkham.Act.Cards.TheRelicIsMissing
  ( TheRelicIsMissing(..)
  , theRelicIsMissing
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype TheRelicIsMissing = TheRelicIsMissing ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theRelicIsMissing :: ActCard TheRelicIsMissing
theRelicIsMissing =
  act (1, A) TheRelicIsMissing Cards.theRelicIsMissing Nothing

instance RunMessage TheRelicIsMissing where
  runMessage msg (TheRelicIsMissing attrs) =
    TheRelicIsMissing <$> runMessage msg attrs
