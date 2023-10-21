module Arkham.Treachery.Cards.ChillingPresence
  ( chillingPresence
  , ChillingPresence(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChillingPresence = ChillingPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chillingPresence :: TreacheryCard ChillingPresence
chillingPresence = treachery ChillingPresence Cards.chillingPresence

instance RunMessage ChillingPresence where
  runMessage msg t@(ChillingPresence attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> ChillingPresence <$> runMessage msg attrs
