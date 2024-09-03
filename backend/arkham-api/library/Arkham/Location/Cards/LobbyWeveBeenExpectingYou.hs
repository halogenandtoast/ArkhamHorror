module Arkham.Location.Cards.LobbyWeveBeenExpectingYou (
  lobbyWeveBeenExpectingYou,
  LobbyWeveBeenExpectingYou (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message

newtype LobbyWeveBeenExpectingYou = LobbyWeveBeenExpectingYou LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyWeveBeenExpectingYou :: LocationCard LobbyWeveBeenExpectingYou
lobbyWeveBeenExpectingYou = location LobbyWeveBeenExpectingYou Cards.lobbyWeveBeenExpectingYou 3 (Static 0)

instance HasModifiersFor LobbyWeveBeenExpectingYou where
  getModifiersFor target (LobbyWeveBeenExpectingYou attrs)
    | isTarget attrs target =
        pure $ toModifiers attrs [Blocked | not (locationRevealed attrs)]
  getModifiersFor _ _ = pure []

instance HasAbilities LobbyWeveBeenExpectingYou where
  getAbilities (LobbyWeveBeenExpectingYou attrs) =
    withBaseAbilities
      attrs
      [ withTooltip
        "{action}: _Parley._ The guards recognize you from the Meiger estate and let you pass. Reveal the Lobby."
        $ restrictedAbility
          (proxied (LocationMatcherSource $ LocationWithTitle "Lodge Gates") attrs)
          1
          (OnLocation $ LocationWithTitle "Lodge Gates")
          (ActionAbility [#parley] $ ActionCost 1)
      | unrevealed attrs
      ]

instance RunMessage LobbyWeveBeenExpectingYou where
  runMessage msg l@(LobbyWeveBeenExpectingYou attrs) = case msg of
    UseCardAbility iid (ProxySource _ source) 1 _ _
      | isSource attrs source && unrevealed attrs -> do
          push $ RevealLocation (Just iid) (toId attrs)
          pure l
    _ -> LobbyWeveBeenExpectingYou <$> runMessage msg attrs
