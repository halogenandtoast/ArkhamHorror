module Arkham.Treachery.Cards.InconvenientQuesitoningC (inconvenientQuesitoningC) where

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InconvenientQuesitoningC = InconvenientQuesitoningC TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inconvenientQuesitoningC :: TreacheryCard InconvenientQuesitoningC
inconvenientQuesitoningC = treachery InconvenientQuesitoningC Cards.inconvenientQuesitoningC

instance RunMessage InconvenientQuesitoningC where
  runMessage msg t@(InconvenientQuesitoningC attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> InconvenientQuesitoningC <$> liftRunMessage msg attrs
