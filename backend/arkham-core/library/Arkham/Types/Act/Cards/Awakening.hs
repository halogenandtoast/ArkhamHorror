module Arkham.Types.Act.Cards.Awakening
  ( Awakening(..)
  , awakening
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype Awakening = Awakening ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

awakening :: ActCard Awakening
awakening = act (1, A) Awakening Cards.awakening Nothing

instance ActRunner env => RunMessage env Awakening where
  runMessage msg (Awakening attrs) = Awakening <$> runMessage msg attrs
