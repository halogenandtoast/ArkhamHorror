module Arkham.Treachery.Cards.Witchweed (witchweed) where

import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Witchweed = Witchweed TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

witchweed :: TreacheryCard Witchweed
witchweed = treachery Witchweed Cards.witchweed

instance RunMessage Witchweed where
  runMessage msg t@(Witchweed attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      case attrs.drawnFrom of
        Just (Deck.EncounterDeckByKey SpectralEncounterDeck) -> do
          runHauntedAbilities iid
          runHauntedAbilities iid
          removeTreachery attrs
          chooseOneM iid $ scenarioI18n do
            labeled' "witchweed.surge" do
              gainSurge attrs
              addToSpecificEncounterDiscard RegularEncounterDeck (only $ toCard attrs)
            labeled' "witchweed.otherwise" do
              addToSpecificEncounterDiscard SpectralEncounterDeck (only $ toCard attrs)
        _ -> do
          removeTreachery attrs
          shuffleCardsIntoDeck SpectralEncounterDeck [toCard attrs]
      pure t
    _ -> Witchweed <$> liftRunMessage msg attrs
