module Arkham.Location.Cards.Lobby (lobby) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Lobby = Lobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobby :: LocationCard Lobby
lobby = location Lobby Cards.lobby 4 (Static 1)

instance HasAbilities Lobby where
  getAbilities (Lobby a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #when Anyone (be a)
      , restricted a 2 (Here <> can.draw.cards You) doubleActionAbility
      ]

instance RunMessage Lobby where
  runMessage msg l@(Lobby attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      lobbyDoorways <- sampleListN 2 =<< getSetAsideCardsMatching "Lobby Doorway"
      placeLabeledLocations_ "lobbyDoorway" lobbyDoorways
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCards iid (attrs.ability 2) 3
      pure l
    _ -> Lobby <$> liftRunMessage msg attrs
