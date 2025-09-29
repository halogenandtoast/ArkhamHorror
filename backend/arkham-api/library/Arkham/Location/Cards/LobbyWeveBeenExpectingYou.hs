module Arkham.Location.Cards.LobbyWeveBeenExpectingYou (lobbyWeveBeenExpectingYou) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ForTheGreaterGood.Helpers

newtype LobbyWeveBeenExpectingYou = LobbyWeveBeenExpectingYou LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyWeveBeenExpectingYou :: LocationCard LobbyWeveBeenExpectingYou
lobbyWeveBeenExpectingYou = location LobbyWeveBeenExpectingYou Cards.lobbyWeveBeenExpectingYou 3 (Static 0)

instance HasModifiersFor LobbyWeveBeenExpectingYou where
  getModifiersFor (LobbyWeveBeenExpectingYou a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities LobbyWeveBeenExpectingYou where
  getAbilities (LobbyWeveBeenExpectingYou a) =
    extendUnrevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "lobbyWeveBeenExpectingYou.parley"
      $ restricted
        (proxied (LocationMatcherSource "Lodge Gates") a)
        1
        (OnLocation "Lodge Gates")
        parleyAction_

instance RunMessage LobbyWeveBeenExpectingYou where
  runMessage msg l@(LobbyWeveBeenExpectingYou attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      revealBy iid attrs
      pure l
    _ -> LobbyWeveBeenExpectingYou <$> liftRunMessage msg attrs
