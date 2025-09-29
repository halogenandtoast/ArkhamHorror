module Arkham.Location.Cards.LobbyMembersOnly (lobbyMembersOnly) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype LobbyMembersOnly = LobbyMembersOnly LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyMembersOnly :: LocationCard LobbyMembersOnly
lobbyMembersOnly = location LobbyMembersOnly Cards.lobbyMembersOnly 3 (PerPlayer 1)

instance HasAbilities LobbyMembersOnly where
  getAbilities (LobbyMembersOnly a) =
    extendRevealed1 a $ mkAbility a 1 $ forced $ RevealLocation #after You (be a)

instance RunMessage LobbyMembersOnly where
  runMessage msg l@(LobbyMembersOnly attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      getRandomKey >>= traverse_ (placeKey attrs)
      pure l
    _ -> LobbyMembersOnly <$> liftRunMessage msg attrs
