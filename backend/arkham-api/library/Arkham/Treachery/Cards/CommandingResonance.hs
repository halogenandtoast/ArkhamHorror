module Arkham.Treachery.Cards.CommandingResonance (commandingResonance) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype CommandingResonance = CommandingResonance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

commandingResonance :: TreacheryCard CommandingResonance
commandingResonance = treachery CommandingResonance Cards.commandingResonance

instance RunMessage CommandingResonance where
  runMessage msg t@(CommandingResonance attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> CommandingResonance <$> liftRunMessage msg attrs
