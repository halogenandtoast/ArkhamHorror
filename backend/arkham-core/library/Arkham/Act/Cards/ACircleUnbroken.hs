module Arkham.Act.Cards.ACircleUnbroken
  ( ACircleUnbroken(..)
  , aCircleUnbroken
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype ACircleUnbroken = ACircleUnbroken ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

aCircleUnbroken :: ActCard ACircleUnbroken
aCircleUnbroken = act (4, A) ACircleUnbroken Cards.aCircleUnbroken Nothing

instance RunMessage ACircleUnbroken where
  runMessage msg (ACircleUnbroken attrs) = ACircleUnbroken <$> runMessage msg attrs
