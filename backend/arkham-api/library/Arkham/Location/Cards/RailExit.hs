module Arkham.Location.Cards.RailExit (railExit) where

import Arkham.Helpers.Modifiers (modifySelf)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.WrittenInRock.Helpers

newtype RailExit = RailExit LocationAttrs
  deriving anyclass (IsLocation, RunMessage)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

railExit :: LocationCard RailExit
railExit = location RailExit Cards.railExit 1 (Static 0)

instance HasModifiersFor RailExit where
  getModifiersFor (RailExit a) = modifySelf a [CannotBeSlidOrSwapped]
