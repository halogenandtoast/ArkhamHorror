module Arkham.Event.Cards.ControlVariable (controlVariable, ControlVariable (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted

newtype ControlVariable = ControlVariable EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

controlVariable :: EventCard ControlVariable
controlVariable = event ControlVariable Cards.controlVariable

instance RunMessage ControlVariable where
  runMessage msg e@(ControlVariable attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      discoverAtYourLocation NotInvestigate iid attrs 1
      pure e
    _ -> ControlVariable <$> liftRunMessage msg attrs
