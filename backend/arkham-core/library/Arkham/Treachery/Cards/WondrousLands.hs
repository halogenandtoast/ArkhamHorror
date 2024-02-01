module Arkham.Treachery.Cards.WondrousLands ( wondrousLands , WondrousLands(..)) where

import Arkham.Prelude
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WondrousLands = WondrousLands TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

wondrousLands :: TreacheryCard WondrousLands
wondrousLands = treachery WondrousLands Cards.wondrousLands

instance RunMessage WondrousLands where
  runMessage msg t@(WondrousLands attrs) = case msg of
    Revelation _iid (isSource attrs -> True) -> pure t
    _ -> WondrousLands <$> runMessage msg attrs
