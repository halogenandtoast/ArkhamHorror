module Arkham.Homebrew.CircusExMortis.Agendas.HouseOfHorrors (houseOfHorrors) where

import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Helpers (campaignI18n)
import Arkham.ChaosToken
import Arkham.Helpers.ChaosBag (getOnlyChaosTokensInBag)
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCard)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype HouseOfHorrors = HouseOfHorrors AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

houseOfHorrors :: AgendaCard HouseOfHorrors
houseOfHorrors =
  agenda (2, A) HouseOfHorrors Cards.houseOfHorrors (Static 5)

instance RunMessage HouseOfHorrors where
  runMessage msg a@(HouseOfHorrors attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) ->
      campaignI18n $ scope "oneNightOnly" $ scope "houseOfHorrors" do
        eachInvestigator \iid -> do
          hand <- fieldMap InvestigatorHand length iid
          moons <- filter ((== MoonToken) . (.face)) <$> getOnlyChaosTokensInBag
          chooseOneM iid do
            labeled' "discardHalfHand" $ replicateM_ ((hand + 1) `div` 2) (chooseAndDiscardCard iid attrs)
            when (notNull moons) do
              labeled' "sealMoonToken" $ for_ (take 1 moons) \token -> sealChaosToken iid iid token
        advanceAgendaDeck attrs
        pure a
    _ -> HouseOfHorrors <$> liftRunMessage msg attrs
