module Arkham.Treachery.Cards.Damned (
  damned,
  Damned (..),
)
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Damned = Damned TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

damned :: TreacheryCard Damned
damned = treachery Damned Cards.damned

instance RunMessage Damned where
  runMessage msg (Damned attrs) = Damned <$> runMessage msg attrs
