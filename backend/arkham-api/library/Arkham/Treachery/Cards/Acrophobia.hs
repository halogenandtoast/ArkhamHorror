module Arkham.Treachery.Cards.Acrophobia (acrophobia) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Acrophobia = Acrophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

acrophobia :: TreacheryCard Acrophobia
acrophobia = treachery Acrophobia Cards.acrophobia

-- TODO: abilities
instance RunMessage Acrophobia where
  runMessage msg (Acrophobia attrs) = runQueueT $ Acrophobia <$> liftRunMessage msg attrs
