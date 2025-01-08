module Arkham.Agenda.Cards.TheChaseIsOnV2 (TheChaseIsOnV2 (..), theChaseIsOnV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.Matcher
import Arkham.Scenarios.HorrorInHighGear.Helpers
import Arkham.Trait (Trait (Vehicle))
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheChaseIsOnV2 = TheChaseIsOnV2 AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theChaseIsOnV2 :: AgendaCard TheChaseIsOnV2
theChaseIsOnV2 = agenda (1, A) TheChaseIsOnV2 Cards.theChaseIsOnV2 (Static 8)

instance HasModifiersFor TheChaseIsOnV2 where
  getModifiersFor (TheChaseIsOnV2 a) = do
    modifySelect a (not_ $ InVehicleMatching AnyAsset) [AdditionalActionCostOf #move 2]

instance HasAbilities TheChaseIsOnV2 where
  getAbilities (TheChaseIsOnV2 a) = [mkAbility a 1 $ forced $ RoundEnds #when]

instance RunMessage TheChaseIsOnV2 where
  runMessage msg a@(TheChaseIsOnV2 attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      lead <- getLead
      selectEach (AssetWithTrait Vehicle <> AssetWithSubtitle "Running") (flipOver lead)
      shuffleEncounterDiscardBackIn
      n <- getPlayerCount
      when (n >= 3) do
        discardUntilFirst lead attrs Deck.EncounterDeck
          $ basic (oneOf [#enemy <> withTrait Vehicle, cardIs Treacheries.malfunction])

      advanceAgendaDeck attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advanceRoad
      pure a
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just card) -> do
      drawCard iid card
      pure a
    _ -> TheChaseIsOnV2 <$> liftRunMessage msg attrs
