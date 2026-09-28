module Arkham.Homebrew.CircusExMortis.Agendas.RepeatShowing (repeatShowing) where

import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.FlavorText (flavor, p, setTitle)
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers
import Arkham.I18n

newtype RepeatShowing = RepeatShowing AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

repeatShowing :: AgendaCard RepeatShowing
repeatShowing = agenda (1, A) RepeatShowing Cards.repeatShowing (Static 6)

instance HasAbilities RepeatShowing where
  getAbilities (RepeatShowing a) = [replenishBigTopRingsAbility a]

instance RunMessage RepeatShowing where
  runMessage msg a@(RepeatShowing attrs) =
    runQueueT $ scenarioI18n "piperAtTheGatesOfDawn" $ scope "repeatShowing" $ case msg of
      UseThisAbility _ (isSource attrs -> True) 1 -> do
        replenishBigTopRings (attrs.ability 1)
        pure a
      AdvanceAgenda (isSide B attrs -> True) -> do
        flavor $ setTitle "title" >> p "body"
        forEachInvestigator (push msg)
        advanceAgendaDeck attrs
        pure a
      ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
        sealMoonTokenOrLoseAction attrs iid
        pure a
      _ -> RepeatShowing <$> liftRunMessage msg attrs
