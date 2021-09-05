module Arkham.Types.Act.Cards.TheStrangerThePathIsMine
  ( TheStrangerThePathIsMine(..)
  , theStrangerThePathIsMine
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes

newtype TheStrangerThePathIsMine = TheStrangerThePathIsMine ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerThePathIsMine :: ActCard TheStrangerThePathIsMine
theStrangerThePathIsMine =
  act (2, A) TheStrangerThePathIsMine Cards.theStrangerThePathIsMine Nothing

instance ActRunner env => RunMessage env TheStrangerThePathIsMine where
  runMessage msg (TheStrangerThePathIsMine attrs) =
    TheStrangerThePathIsMine <$> runMessage msg attrs
