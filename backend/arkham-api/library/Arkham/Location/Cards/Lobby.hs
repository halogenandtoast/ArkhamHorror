module Arkham.Location.Cards.Lobby (lobby, Lobby (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype Lobby = Lobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobby :: LocationCard Lobby
lobby = location Lobby Cards.lobby 4 (Static 1)

instance HasAbilities Lobby where
  getAbilities (Lobby attrs) =
    extendRevealed
      attrs
      [ restrictedAbility attrs 1 CanDrawCards $ forced $ RevealLocation #when Anyone (be attrs)
      , restrictedAbility attrs 2 Here $ ActionAbility [] $ ActionCost 2
      ]

instance RunMessage Lobby where
  runMessage msg l@(Lobby attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lobbyDoorwayCount <- selectCount $ LocationWithUnrevealedTitle "Lobby Doorway"
      lobbyDoorways <-
        zip [lobbyDoorwayCount ..]
          . take 2
          <$> (shuffleM =<< select (SetAsideCardMatch "Lobby Doorway"))
      msgs <- for lobbyDoorways \(idx, lobbyDoorway) -> do
        (locationId, placement) <- placeLocation lobbyDoorway
        pure [placement, SetLocationLabel locationId $ "lobbyDoorway" <> tshow (idx + 1)]
      pushAll $ concat msgs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ drawCards iid (attrs.ability 2) 3
      pure l
    _ -> Lobby <$> runMessage msg attrs
