module Arkham.Location.Cards.LobbyWeveBeenExpectingYou (
  lobbyWeveBeenExpectingYou,
  LobbyWeveBeenExpectingYou (..),
)
where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message

newtype LobbyWeveBeenExpectingYou = LobbyWeveBeenExpectingYou LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyWeveBeenExpectingYou :: LocationCard LobbyWeveBeenExpectingYou
lobbyWeveBeenExpectingYou = location LobbyWeveBeenExpectingYou Cards.lobbyWeveBeenExpectingYou 3 (Static 0)

instance HasModifiersFor LobbyWeveBeenExpectingYou where
  getModifiersFor (LobbyWeveBeenExpectingYou a) = whenUnrevealed a $ modifySelf a [Blocked]

instance HasAbilities LobbyWeveBeenExpectingYou where
  getAbilities (LobbyWeveBeenExpectingYou attrs) =
    extend
      attrs
      [ withTooltip
        "{action}: _Parley._ The guards recognize you from the Meiger estate and let you pass. Reveal the Lobby."
        $ restricted
          (proxied (LocationMatcherSource "Lodge Gates") attrs)
          1
          (OnLocation "Lodge Gates")
          parleyAction_
      | attrs.unrevealed
      ]

instance RunMessage LobbyWeveBeenExpectingYou where
  runMessage msg l@(LobbyWeveBeenExpectingYou attrs) = runQueueT $ case msg of
    UseThisAbility iid (isProxySource attrs -> True) 1 -> do
      push $ RevealLocation (Just iid) (toId attrs)
      pure l
    _ -> LobbyWeveBeenExpectingYou <$> liftRunMessage msg attrs
