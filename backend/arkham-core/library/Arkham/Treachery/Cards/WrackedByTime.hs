module Arkham.Treachery.Cards.WrackedByTime
  ( wrackedByTime
  , WrackedByTime(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype WrackedByTime = WrackedByTime TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByTime :: TreacheryCard WrackedByTime
wrackedByTime = treachery WrackedByTime Cards.wrackedByTime

instance RunMessage WrackedByTime where
  runMessage msg t@(WrackedByTime attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> WrackedByTime <$> runMessage msg attrs
