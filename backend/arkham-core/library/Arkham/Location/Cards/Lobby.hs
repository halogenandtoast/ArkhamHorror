module Arkham.Location.Cards.Lobby
  ( lobby
  , Lobby(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Timing qualified as Timing

newtype Lobby = Lobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobby :: LocationCard Lobby
lobby = location Lobby Cards.lobby 4 (Static 1)

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

instance RunMessage Lobby where
  runMessage msg l@(Lobby attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      lobbyDoorwayCount <- selectCount
        (LocationWithUnrevealedTitle "Lobby Doorway")
      lobbyDoorways <-
        zip [lobbyDoorwayCount ..]
        . take 2
        <$> (shuffleM =<< selectList
              (SetAsideCardMatch $ CardWithTitle "Lobby Doorway")
            )
      msgs <- for lobbyDoorways \(idx, lobbyDoorway) -> do
        (locationId, placement) <- placeLocation lobbyDoorway
        pure [placement, SetLocationLabel locationId $ "lobbyDoorway" <> tshow (idx + 1)]
      pushAll $ concat msgs
      pure l
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 3
      push drawing
      pure l
    _ -> Lobby <$> runMessage msg attrs
