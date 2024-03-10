module Arkham.Act.Cards.UnexpectedRescue
  ( UnexpectedRescue(..)
  , unexpectedRescue
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype UnexpectedRescue = UnexpectedRescue ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

unexpectedRescue :: ActCard UnexpectedRescue
unexpectedRescue = act (4, A) UnexpectedRescue Cards.unexpectedRescue Nothing

instance RunMessage UnexpectedRescue where
  runMessage msg (UnexpectedRescue attrs) = UnexpectedRescue <$> runMessage msg attrs
