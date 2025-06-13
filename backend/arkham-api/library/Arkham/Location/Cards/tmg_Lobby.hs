module Arkham.Location.Cards.TMGLobby (tmgLobby) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import

newtype TMGLobby = TMGLobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tmgLobby :: LocationCard TMGLobby
tmgLobby = location TMGLobby Cards.tmgLobby 2 (Static 0)

instance RunMessage TMGLobby where
  runMessage msg (TMGLobby attrs) =
    TMGLobby <$> runMessage msg attrs
