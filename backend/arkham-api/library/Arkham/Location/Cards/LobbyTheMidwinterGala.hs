module Arkham.Location.Cards.LobbyTheMidwinterGala (lobbyTheMidwinterGala) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.TheMidwinterGala.Helpers

newtype LobbyTheMidwinterGala = LobbyTheMidwinterGala LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobbyTheMidwinterGala :: LocationCard LobbyTheMidwinterGala
lobbyTheMidwinterGala = location LobbyTheMidwinterGala Cards.lobbyTheMidwinterGala 2 (PerPlayer 1)

instance HasAbilities LobbyTheMidwinterGala where
  getAbilities (LobbyTheMidwinterGala a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted a 1 (Here <> exists (mapOneOf AgendaWithStep [2, 3]))
      $ FastAbility (CalculatedResourceCost $ GameValueCalculation $ PerPlayer 1)

instance RunMessage LobbyTheMidwinterGala where
  runMessage msg l@(LobbyTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      topOfGuestDeck <- take 1 <$> getGuestDeck
      for_ topOfGuestDeck \card -> do
        obtainCard card
        createAssetAt_ card (AtLocation attrs.id)
      pure l
    _ -> LobbyTheMidwinterGala <$> liftRunMessage msg attrs
