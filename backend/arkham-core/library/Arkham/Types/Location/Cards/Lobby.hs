module Arkham.Types.Location.Cards.Lobby
  ( lobby
  , Lobby(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import qualified Arkham.Types.Timing as Timing

newtype Lobby = Lobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobby :: LocationCard Lobby
lobby = location Lobby Cards.lobby 4 (Static 1) Triangle [Circle, Square, Plus]

instance HasAbilities Lobby where
  getAbilities (Lobby attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility attrs 1 CanDrawCards
        $ ForcedAbility
        $ RevealLocation Timing.When Anyone
        $ LocationWithId
        $ toId attrs
        , restrictedAbility attrs 1 Here $ ActionAbility Nothing $ ActionCost 2
        ]
      else []

instance LocationRunner env => RunMessage env Lobby where
  runMessage msg l@(Lobby attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      lobbyDoorwayCount <- selectCount
        (LocationWithUnrevealedTitle "Lobby Doorway")
      lobbyDoorways <- zip [lobbyDoorwayCount ..]
        <$> selectList (SetAsideCardMatch $ CardWithTitle "Lobby Doorway")
      msgs <- concat <$> for
        lobbyDoorways
        \(idx, lobbyDoorway) -> do
          locationId <- getRandom
          pure
            [ PlaceLocation locationId (toCardDef lobbyDoorway)
            , SetLocationLabel locationId $ "lobbyDoorway" <> tshow (idx + 1)
            ]
      l <$ pushAll msgs
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      l <$ push (DrawCards iid 3 False)
    _ -> Lobby <$> runMessage msg attrs
