module Arkham.Treachery.Cards.Apeirophobia ( apeirophobia , Apeirophobia(..)) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Apeirophobia = Apeirophobia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apeirophobia :: TreacheryCard Apeirophobia
apeirophobia = treachery Apeirophobia Cards.apeirophobia

instance RunMessage Apeirophobia where
  runMessage msg t@(Apeirophobia attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Apeirophobia <$> liftRunMessage msg attrs
