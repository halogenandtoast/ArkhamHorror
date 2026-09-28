module Arkham.Homebrew.CircusExMortis.Agendas.WhirlingSpectacle (whirlingSpectacle) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n
import Arkham.Message.Lifted.Choose

newtype WhirlingSpectacle = WhirlingSpectacle AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whirlingSpectacle :: AgendaCard WhirlingSpectacle
whirlingSpectacle = agenda (3, A) WhirlingSpectacle Cards.whirlingSpectacle (Static 6)

instance HasAbilities WhirlingSpectacle where
  getAbilities (WhirlingSpectacle a) = [replenishBigTopRingsAbility a]

instance RunMessage WhirlingSpectacle where
  runMessage msg a@(WhirlingSpectacle attrs) =
    runQueueT $ scenarioI18n "piperAtTheGatesOfDawn" $ scope "whirlingSpectacle" $ case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        replenishBigTopRings (attrs.ability 1)
        pure a
      -- resigned investigators are already eliminated, so 'eachInvestigator' skips them
      AdvanceAgenda (isSide B attrs -> True) -> do
        eachInvestigator \iid -> do
          chooseOneM iid do
            labeled "physicalTrauma" $ sufferPhysicalTrauma iid 1
            labeled "mentalTrauma" $ sufferMentalTrauma iid 1
          investigatorDefeated attrs iid
        pure a
      _ -> WhirlingSpectacle <$> liftRunMessage msg attrs
