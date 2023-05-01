module Arkham.Treachery.Cards.Punishment
  ( punishment
  , Punishment(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Punishment = Punishment TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

punishment :: TreacheryCard Punishment
punishment = treachery Punishment Cards.punishment

instance RunMessage Punishment where
  runMessage msg t@(Punishment attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> Punishment <$> runMessage msg attrs
