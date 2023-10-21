module Arkham.Treachery.Cards.MorbidAwareness
  ( morbidAwareness
  , MorbidAwareness(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype MorbidAwareness = MorbidAwareness TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

morbidAwareness :: TreacheryCard MorbidAwareness
morbidAwareness = treachery MorbidAwareness Cards.morbidAwareness

instance RunMessage MorbidAwareness where
  runMessage msg t@(MorbidAwareness attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> MorbidAwareness <$> runMessage msg attrs
