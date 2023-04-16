module Arkham.Treachery.Cards.GhostlyPresence
  ( ghostlyPresence
  , GhostlyPresence(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype GhostlyPresence = GhostlyPresence TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ghostlyPresence :: TreacheryCard GhostlyPresence
ghostlyPresence = treachery GhostlyPresence Cards.ghostlyPresence

instance RunMessage GhostlyPresence where
  runMessage msg t@(GhostlyPresence attrs) = case msg of
    Revelation _iid source | isSource attrs source -> pure t
    _ -> GhostlyPresence <$> runMessage msg attrs
