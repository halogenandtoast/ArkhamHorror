module Arkham.Act.Cards.MagicAndScience
  ( MagicAndScience(..)
  , magicAndScience
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype MagicAndScience = MagicAndScience ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

magicAndScience :: ActCard MagicAndScience
magicAndScience = act (2, A) MagicAndScience Cards.magicAndScience Nothing

instance RunMessage MagicAndScience where
  runMessage msg (MagicAndScience attrs) =
    MagicAndScience <$> runMessage msg attrs
