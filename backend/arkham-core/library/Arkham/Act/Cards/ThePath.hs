module Arkham.Act.Cards.ThePath
  ( ThePath(..)
  , thePath
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ThePath = ThePath ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

thePath :: ActCard ThePath
thePath = act (4, A) ThePath Cards.thePath Nothing

instance RunMessage ThePath where
  runMessage msg (ThePath attrs) = ThePath <$> runMessage msg attrs
