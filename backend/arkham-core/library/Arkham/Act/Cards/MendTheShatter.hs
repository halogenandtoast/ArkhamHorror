module Arkham.Act.Cards.MendTheShatter
  ( MendTheShatter(..)
  , mendTheShatter
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype MendTheShatter = MendTheShatter ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

mendTheShatter :: ActCard MendTheShatter
mendTheShatter = act (4, A) MendTheShatter Cards.mendTheShatter Nothing

instance RunMessage MendTheShatter where
  runMessage msg (MendTheShatter attrs) =
    MendTheShatter <$> runMessage msg attrs
