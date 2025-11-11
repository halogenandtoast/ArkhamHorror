module Arkham.Treachery.Cards.InconvenientQuesitoningD (inconvenientQuesitoningD) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InconvenientQuesitoningD = InconvenientQuesitoningD TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconvenientQuesitoningD :: TreacheryCard InconvenientQuesitoningD
inconvenientQuesitoningD = treachery InconvenientQuesitoningD Cards.inconvenientQuesitoningD

instance RunMessage InconvenientQuesitoningD where
  runMessage msg t@(InconvenientQuesitoningD attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> InconvenientQuesitoningD <$> liftRunMessage msg attrs
