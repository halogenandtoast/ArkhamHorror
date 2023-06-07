module Arkham.Location.Cards.LobbyMembersOnly
  ( lobbyMembersOnly
  , LobbyMembersOnly(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LobbyMembersOnly = LobbyMembersOnly LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyMembersOnly :: LocationCard LobbyMembersOnly
lobbyMembersOnly = location LobbyMembersOnly Cards.lobbyMembersOnly 3 (PerPlayer 1)

instance HasAbilities LobbyMembersOnly where
  getAbilities (LobbyMembersOnly attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LobbyMembersOnly where
  runMessage msg (LobbyMembersOnly attrs) =
    LobbyMembersOnly <$> runMessage msg attrs
