module Arkham.Act.Cards.HarlansCurseSafekeeping
  ( HarlansCurseSafekeeping(..)
  , harlansCurseSafekeeping
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype HarlansCurseSafekeeping = HarlansCurseSafekeeping ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harlansCurseSafekeeping :: ActCard HarlansCurseSafekeeping
harlansCurseSafekeeping =
  act (2, A) HarlansCurseSafekeeping Cards.harlansCurseSafekeeping Nothing

instance RunMessage HarlansCurseSafekeeping where
  runMessage msg (HarlansCurseSafekeeping attrs) =
    HarlansCurseSafekeeping <$> runMessage msg attrs
