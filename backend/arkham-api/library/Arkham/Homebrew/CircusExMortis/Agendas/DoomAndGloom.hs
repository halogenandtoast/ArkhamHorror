module Arkham.Homebrew.CircusExMortis.Agendas.DoomAndGloom (doomAndGloom) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers

newtype DoomAndGloom = DoomAndGloom AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doomAndGloom :: AgendaCard DoomAndGloom
doomAndGloom = agenda (2, A) DoomAndGloom Cards.doomAndGloom (Static 6)

instance HasAbilities DoomAndGloom where
  getAbilities (DoomAndGloom a) = [replenishBigTopRingsAbility a]

instance RunMessage DoomAndGloom where
  runMessage msg a@(DoomAndGloom attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      replenishBigTopRings (attrs.ability 1)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      forEachInvestigator (push msg)
      advanceAgendaDeck attrs
      pure a
    ForInvestigator iid (AdvanceAgenda (isSide B attrs -> True)) -> do
      sealMoonTokenOrLoseAction attrs iid
      pure a
    _ -> DoomAndGloom <$> liftRunMessage msg attrs
