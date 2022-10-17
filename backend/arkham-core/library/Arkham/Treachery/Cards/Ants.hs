module Arkham.Treachery.Cards.Ants
  ( ants
  , Ants(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Ants = Ants TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ants :: TreacheryCard Ants
ants = treachery Ants Cards.ants

instance RunMessage Ants where
  runMessage msg t@(Ants attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> Ants <$> runMessage msg attrs
