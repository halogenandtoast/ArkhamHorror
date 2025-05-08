module Arkham.Agenda.Cards.BetterNeverThanLate (betterNeverThanLate) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Asset.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
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
      locations <-
        select
          $ LocationWithAsset
          $ AssetWithFewestClues
          $ AssetWithTrait Bystander
          <> AssetWithClues (atLeast 1)
      leadChooseOneM do
        targets locations (createAssetAt_ Cards.dianneDevineHidingAnOathUnspoken . AtLocation)

      shuffleSetAsideIntoEncounterDeck $ cardIs Treacheries.shockingDisplay
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> BetterNeverThanLate <$> liftRunMessage msg attrs
