module Arkham.Act.Cards.HarlansCurseHarlanEarnstone
  ( HarlansCurseHarlanEarnstone(..)
  , harlansCurseHarlanEarnstone
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes

newtype HarlansCurseHarlanEarnstone = HarlansCurseHarlanEarnstone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harlansCurseHarlanEarnstone :: ActCard HarlansCurseHarlanEarnstone
harlansCurseHarlanEarnstone = act
  (2, A)
  HarlansCurseHarlanEarnstone
  Cards.harlansCurseHarlanEarnstone
  Nothing

instance RunMessage HarlansCurseHarlanEarnstone where
  runMessage msg (HarlansCurseHarlanEarnstone attrs) =
    HarlansCurseHarlanEarnstone <$> runMessage msg attrs
