module Arkham.Treachery.Cards.InconvenientQuesitoningB (inconvenientQuesitoningB) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InconvenientQuesitoningB = InconvenientQuesitoningB TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconvenientQuesitoningB :: TreacheryCard InconvenientQuesitoningB
inconvenientQuesitoningB = treachery InconvenientQuesitoningB Cards.inconvenientQuesitoningB

instance RunMessage InconvenientQuesitoningB where
  runMessage msg t@(InconvenientQuesitoningB attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> InconvenientQuesitoningB <$> liftRunMessage msg attrs
