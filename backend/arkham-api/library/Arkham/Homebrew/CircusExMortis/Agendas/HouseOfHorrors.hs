module Arkham.Homebrew.CircusExMortis.Agendas.HouseOfHorrors (houseOfHorrors) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (moonToken, scenarioI18n, sealMoonTokenOn)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype HouseOfHorrors = HouseOfHorrors AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseOfHorrors :: AgendaCard HouseOfHorrors
houseOfHorrors = agenda (2, A) HouseOfHorrors Cards.houseOfHorrors (Static 5)

instance RunMessage HouseOfHorrors where
  runMessage msg a@(HouseOfHorrors attrs) = runQueueT $ scenarioI18n "oneNightOnly" $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (`forInvestigator` msg)
      advanceAgendaDeck attrs
      pure a
    -- deferred per investigator: sealing removes a ☾ from the bag, so the next
    -- investigator's options must be recomputed against the bag as it now stands
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> scope "houseOfHorrors" do
      hand <- fieldMap InvestigatorHand length iid
      moonInBag <- selectAny moonToken
      chooseOneM iid do
        labeled' "discardHalfHand" $ replicateM_ ((hand + 1) `div` 2) (chooseAndDiscardCard iid attrs)
        when moonInBag $ labeled' "sealMoonToken" $ sealMoonTokenOn iid
      pure a
    _ -> HouseOfHorrors <$> liftRunMessage msg attrs
