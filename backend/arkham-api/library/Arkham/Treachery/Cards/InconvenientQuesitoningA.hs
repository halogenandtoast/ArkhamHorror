module Arkham.Treachery.Cards.InconvenientQuesitoningA (inconvenientQuesitoningA) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InconvenientQuesitoningA = InconvenientQuesitoningA TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconvenientQuesitoningA :: TreacheryCard InconvenientQuesitoningA
inconvenientQuesitoningA = treachery InconvenientQuesitoningA Cards.inconvenientQuesitoningA

instance RunMessage InconvenientQuesitoningA where
  runMessage msg t@(InconvenientQuesitoningA attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> InconvenientQuesitoningA <$> liftRunMessage msg attrs
