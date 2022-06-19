module Arkham.Act.Cards.OpenThePathAbove
  ( OpenThePathAbove(..)
  , openThePathAbove
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype OpenThePathAbove = OpenThePathAbove ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openThePathAbove :: ActCard OpenThePathAbove
openThePathAbove = act (1, A) OpenThePathAbove Cards.openThePathAbove Nothing

instance RunMessage OpenThePathAbove where
  runMessage msg (OpenThePathAbove attrs) = OpenThePathAbove <$> runMessage msg attrs
