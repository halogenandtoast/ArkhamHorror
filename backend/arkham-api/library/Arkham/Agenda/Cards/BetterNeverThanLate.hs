module Arkham.Agenda.Cards.BetterNeverThanLate (betterNeverThanLate) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Trait
import Arkham.Treachery.Cards qualified as Treacheries

newtype BetterNeverThanLate = BetterNeverThanLate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

betterNeverThanLate :: AgendaCard BetterNeverThanLate
betterNeverThanLate = agenda (1, A) BetterNeverThanLate Cards.betterNeverThanLate (Static 3)

instance RunMessage BetterNeverThanLate where
  runMessage msg a@(BetterNeverThanLate attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      ok <- selectAny $ AssetWithTrait Bystander <> AssetWithClues (atLeast 1)
      when ok do
        createEnemyAtLocationMatching_ Cards.dianneDevineHidingAnOathUnspoken
          $ LocationWithAsset
          $ AssetWithFewestClues
          $ AssetWithTrait Bystander
          <> AssetWithClues (atLeast 1)

      shuffleSetAsideIntoEncounterDeck $ cardIs Treacheries.shockingDisplay
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> BetterNeverThanLate <$> liftRunMessage msg attrs
