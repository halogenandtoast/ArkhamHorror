module Arkham.Location.Cards.LobbyWeveBeenExpectingYou
  ( lobbyWeveBeenExpectingYou
  , LobbyWeveBeenExpectingYou(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype LobbyWeveBeenExpectingYou = LobbyWeveBeenExpectingYou LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyWeveBeenExpectingYou :: LocationCard LobbyWeveBeenExpectingYou
lobbyWeveBeenExpectingYou = location LobbyWeveBeenExpectingYou Cards.lobbyWeveBeenExpectingYou 3 (Static 0)

instance HasAbilities LobbyWeveBeenExpectingYou where
  getAbilities (LobbyWeveBeenExpectingYou attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage LobbyWeveBeenExpectingYou where
  runMessage msg (LobbyWeveBeenExpectingYou attrs) =
    LobbyWeveBeenExpectingYou <$> runMessage msg attrs
