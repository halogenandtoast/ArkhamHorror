module Arkham.Location.Cards.TracesToNowhere.HistoricalSociety (historicalSociety) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Scenario (getEncounterDiscard)
import Arkham.Location.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck

newtype HistoricalSociety = HistoricalSociety LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

historicalSociety :: LocationCard HistoricalSociety
historicalSociety = location HistoricalSociety Cards.historicalSociety 5 (PerPlayer 1)

instance HasAbilities HistoricalSociety where
  getAbilities (HistoricalSociety a) =
    extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage HistoricalSociety where
  runMessage msg l@(HistoricalSociety attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      treacheries <-
        filter (`cardMatch` CardWithType TreacheryType) <$> getEncounterDiscard RegularEncounterDeck
      for_ (listToMaybe treacheries) \card -> do
        push $ GainSurge (attrs.ability 1) (toTarget card)
        push $ InvestigatorDrewEncounterCard iid card
      discoverAtYourLocation NotInvestigate iid (attrs.ability 1) 2
      pure l
    _ -> HistoricalSociety <$> liftRunMessage msg attrs
