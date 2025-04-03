module Arkham.Agenda.Cards.CallOfMadness (callOfMadness) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype CallOfMadness = CallOfMadness AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

callOfMadness :: AgendaCard CallOfMadness
callOfMadness = agenda (3, A) CallOfMadness Cards.callOfMadness (Static 6)

instance HasAbilities CallOfMadness where
  getAbilities (CallOfMadness a) = [restricted a 1 ElectrostaticDetonation $ forced AnyWindow]

instance RunMessage CallOfMadness where
  runMessage msg a@(CallOfMadness attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (`forInvestigator` msg)
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      cards <- getTekelili 3
      chooseOneM iid do
        labeled "Take 1 mental trauma" $ sufferMentalTrauma iid 1
        when (length cards == 3) do
          labeled "Shuffle the top 3 cards of the Tekeli-li deck into their deck" $ addTekelili iid cards
      investigatorDefeated attrs iid
      pure a
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      push R3
      pure a
    _ -> CallOfMadness <$> liftRunMessage msg attrs
