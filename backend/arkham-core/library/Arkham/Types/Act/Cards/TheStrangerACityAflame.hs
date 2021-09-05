module Arkham.Types.Act.Cards.TheStrangerACityAflame
  ( TheStrangerACityAflame(..)
  , theStrangerACityAflame
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype TheStrangerACityAflame = TheStrangerACityAflame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflame :: ActCard TheStrangerACityAflame
theStrangerACityAflame =
  act (2, A) TheStrangerACityAflame Cards.theStrangerACityAflame Nothing

instance ActRunner env => RunMessage env TheStrangerACityAflame where
  runMessage msg (TheStrangerACityAflame attrs) =
    TheStrangerACityAflame <$> runMessage msg attrs
