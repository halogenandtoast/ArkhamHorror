module Arkham.Treachery.Cards.PrimordialGateway
  ( primordialGateway
  , PrimordialGateway(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype PrimordialGateway = PrimordialGateway TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

primordialGateway :: TreacheryCard PrimordialGateway
primordialGateway = treachery PrimordialGateway Cards.primordialGateway

instance RunMessage PrimordialGateway where
  runMessage msg t@(PrimordialGateway attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> PrimordialGateway <$> runMessage msg attrs
