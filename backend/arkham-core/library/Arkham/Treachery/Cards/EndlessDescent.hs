module Arkham.Treachery.Cards.EndlessDescent
  ( endlessDescent
  , EndlessDescent(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype EndlessDescent = EndlessDescent TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

endlessDescent :: TreacheryCard EndlessDescent
endlessDescent = treachery EndlessDescent Cards.endlessDescent

instance RunMessage EndlessDescent where
  runMessage msg t@(EndlessDescent attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> EndlessDescent <$> runMessage msg attrs
