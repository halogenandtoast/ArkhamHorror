module Arkham.Location.Cards.TheMidwinterGala.Lobby (lobby) where

import Arkham.Ability
import Arkham.Calculation
import Arkham.Location.CardDefs.TheMidwinterGala qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheMidwinterGala.Helpers

newtype Lobby = Lobby LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lobby :: LocationCard Lobby
lobby = location Lobby Cards.lobby 2 (PerPlayer 1)

instance HasAbilities Lobby where
  getAbilities (Lobby a) =
    extendRevealed1 a
      $ groupLimit PerRound
      $ restricted
        a
        1
        (Here <> exists (mapOneOf AgendaWithStep [2, 3]) <> ScenarioDeckWithCard GuestDeck)
      $ FastAbility (CalculatedResourceCost $ GameValueCalculation $ PerPlayer 1)

instance RunMessage Lobby where
  runMessage msg l@(Lobby attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      topOfGuestDeck <- take 1 <$> getGuestDeck
      for_ topOfGuestDeck \card -> do
        obtainCard card
        createAssetAt_ card (AtLocation attrs.id)
      pure l
    _ -> Lobby <$> liftRunMessage msg attrs
