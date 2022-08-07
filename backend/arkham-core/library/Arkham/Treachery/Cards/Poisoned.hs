module Arkham.Treachery.Cards.Poisoned
  ( poisoned
  , Poisoned(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Runner

newtype Poisoned = Poisoned TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

poisoned :: TreacheryCard Poisoned
poisoned = treachery Poisoned Cards.poisoned

instance RunMessage Poisoned where
  runMessage msg t@(Poisoned attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Poisoned <$> runMessage msg attrs
