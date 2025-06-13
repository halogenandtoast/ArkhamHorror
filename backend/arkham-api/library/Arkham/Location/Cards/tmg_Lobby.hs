module Arkham.Location.Cards.TMGLobby (tmgLobby) where

import Arkham.Prelude
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Projection
import Arkham.Card
import Arkham.Id

newtype TMGLobby = TMGLobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | 'Lobby' from The Midwinter Gala (#71007).
tmgLobby :: LocationCard TMGLobby
tmgLobby = location
    TMGLobby
    Cards.tmgLobby
    2        -- Shroud
    (PerPlayer 1) -- Clues: 1 per player
    Diamond  -- Symbol
    [Moon, Spade, Triangle, Square, Star] -- Connections
    & revealedBy False

instance HasAbilities TMGLobby where
  getAbilities (TMGLobby attrs) =
    withBaseAbilities attrs $
      [ limitedGroup 1 Timing.When
        $ restrictedAbility attrs 1 OnLocation
        $ costs [Resource 1]
        $ ActionAbility Nothing
      | onAgenda [2, 3] attrs
      ]

instance RunMessage TMGLobby where
  runMessage msg l@(TMGLobby attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      let lid = toId attrs
      pushAll
        [ RevealGuest (toId attrs) -- custom message to reveal Guest card
        , PlaceLocationClues lid 0 -- optional if needed
        ]
      pure l
    _ -> TMGLobby <$> runMessage msg attrs
