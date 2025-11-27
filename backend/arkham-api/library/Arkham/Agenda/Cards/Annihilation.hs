module Arkham.Agenda.Cards.Annihilation (annihilation) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Card
import Arkham.Helpers.Scenario
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Data.Map.Strict qualified as Map

newtype Annihilation = Annihilation AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

annihilation :: AgendaCard Annihilation
annihilation = agenda (3, A) Annihilation Cards.annihilation (Static 5)

instance RunMessage Annihilation where
  runMessage msg a@(Annihilation attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach HollowedCard removeHollow
      doStep 1 msg
      push R4
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      meta <- getScenarioMetaKeyDefault @(Map InvestigatorId [Card]) "removedHollows" mempty
      for_ (Map.assocs meta) \(iid, cards) -> do
        let highestXp = maxes $ cards & map \c -> (c, c.experienceCost)
        focusCards cards do
          chooseTargetM iid highestXp \c -> do
            exile c.id
            when c.unique do
              recordSetInsert ErasedFromExistence [c.cardCode]
              removeCampaignCardFromDeck iid c
      pure a
    _ -> Annihilation <$> liftRunMessage msg attrs
