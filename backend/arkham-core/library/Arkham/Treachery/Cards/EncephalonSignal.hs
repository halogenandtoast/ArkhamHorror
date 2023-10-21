module Arkham.Treachery.Cards.EncephalonSignal
  ( encephalonSignal
  , EncephalonSignal(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EncephalonSignal = EncephalonSignal TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

encephalonSignal :: TreacheryCard EncephalonSignal
encephalonSignal = treachery EncephalonSignal Cards.encephalonSignal

instance RunMessage EncephalonSignal where
  runMessage msg t@(EncephalonSignal attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EncephalonSignal <$> runMessage msg attrs
