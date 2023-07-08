module Arkham.Location.Cards.LobbyMembersOnly (
  lobbyMembersOnly,
  LobbyMembersOnly (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers
import Arkham.Timing qualified as Timing

newtype LobbyMembersOnly = LobbyMembersOnly LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyMembersOnly :: LocationCard LobbyMembersOnly
lobbyMembersOnly = location LobbyMembersOnly Cards.lobbyMembersOnly 3 (PerPlayer 1)

instance HasAbilities LobbyMembersOnly where
  getAbilities (LobbyMembersOnly attrs) =
    withRevealedAbilities
      attrs
      [mkAbility attrs 1 $ ForcedAbility $ RevealLocation Timing.After You $ LocationWithId $ toId attrs]

instance RunMessage LobbyMembersOnly where
  runMessage msg l@(LobbyMembersOnly attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      key <- getRandomKey
      push $ PlaceKey (toTarget attrs) key
      pure l
    _ -> LobbyMembersOnly <$> runMessage msg attrs
