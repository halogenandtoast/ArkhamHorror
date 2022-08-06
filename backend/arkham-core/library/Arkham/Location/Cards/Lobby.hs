module Arkham.Location.Cards.Lobby
  ( lobby
  , Lobby(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Id
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
    UseCardAbility _ source _ 1 _ | isSource attrs source -> do
      lobbyDoorwayCount <- selectCount
        (LocationWithUnrevealedTitle "Lobby Doorway")
      lobbyDoorways <-
        zip [lobbyDoorwayCount ..]
        . take 2
        <$> (shuffleM =<< selectList
              (SetAsideCardMatch $ CardWithTitle "Lobby Doorway")
            )
      msgs <- concat <$> for
        lobbyDoorways
        \(idx, lobbyDoorway) -> pure
          [ PlaceLocation lobbyDoorway
          , SetLocationLabel (LocationId $ toCardId lobbyDoorway)
          $ "lobbyDoorway"
          <> tshow (idx + 1)
          ]
      l <$ pushAll msgs
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      l <$ push (DrawCards iid 3 False)
    _ -> Lobby <$> runMessage msg attrs
