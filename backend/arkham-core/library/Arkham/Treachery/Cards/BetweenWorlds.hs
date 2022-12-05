module Arkham.Treachery.Cards.BetweenWorlds
  ( betweenWorlds
  , BetweenWorlds(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype BetweenWorlds = BetweenWorlds TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

betweenWorlds :: TreacheryCard BetweenWorlds
betweenWorlds = treachery BetweenWorlds Cards.betweenWorlds

instance RunMessage BetweenWorlds where
  runMessage msg t@(BetweenWorlds attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> BetweenWorlds <$> runMessage msg attrs
