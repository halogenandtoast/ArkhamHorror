module Arkham.Agenda.Cards.AroundTheTable (aroundTheTable) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Message.Lifted.Choose

newtype AroundTheTable = AroundTheTable AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aroundTheTable :: AgendaCard AroundTheTable
aroundTheTable = agenda (1, A) AroundTheTable Cards.aroundTheTable (Static 6)

-- TODO (climactic gameplay): the front objective "Confront Mother Rachel! If
-- Mother Rachel is defeated: (→R1)" routes to the scenario resolutions, which
-- are added alongside the full Final Evening scenario.
instance RunMessage AroundTheTable where
  runMessage msg a@(AroundTheTable attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      -- The Sleep: each investigator is defeated and suffers 1 mental or 1
      -- physical trauma.
      eachInvestigator \iid -> do
        chooseOneM iid do
          labeled "Suffer 1 physical trauma" $ sufferPhysicalTrauma iid 1
          labeled "Suffer 1 mental trauma" $ sufferMentalTrauma iid 1
        investigatorDefeated attrs iid
      pure a
    _ -> AroundTheTable <$> liftRunMessage msg attrs
