module Arkham.Location.Cards.Lobby (
  lobby,
  Lobby (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype Lobby = Lobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

lobby :: LocationCard Lobby
lobby = location Lobby Cards.lobby 4 (Static 1)

instance HasAbilities Lobby where
  getAbilities (Lobby attrs) =
    withRevealedAbilities attrs
      $ [ restrictedAbility attrs 1 CanDrawCards
            $ ForcedAbility
            $ RevealLocation Timing.When Anyone
            $ LocationWithId
            $ toId attrs
        , restrictedAbility attrs 2 Here $ ActionAbility [] $ ActionCost 2
        ]

instance RunMessage Lobby where
  runMessage msg l@(Lobby attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      lobbyDoorwayCount <- selectCount $ LocationWithUnrevealedTitle "Lobby Doorway"
      lobbyDoorways <-
        zip [lobbyDoorwayCount ..]
          . take 2
          <$> (shuffleM =<< selectList (SetAsideCardMatch $ CardWithTitle "Lobby Doorway"))
      msgs <- for lobbyDoorways \(idx, lobbyDoorway) -> do
        (locationId, placement) <- placeLocation lobbyDoorway
        pure [placement, SetLocationLabel locationId $ "lobbyDoorway" <> tshow (idx + 1)]
      pushAll $ concat msgs
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 2) 3
      pure l
    _ -> Lobby <$> runMessage msg attrs
