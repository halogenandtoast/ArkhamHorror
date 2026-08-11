module Arkham.Homebrew.DarkMatter.Agendas.FigmentOfYourImagination (figmentOfYourImagination) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards

newtype FigmentOfYourImagination = FigmentOfYourImagination AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Each location is connected to each location adjacent to it." The grid
already makes orthogonally adjacent locations connected, so this clause needs
no implementation.
-}
figmentOfYourImagination :: AgendaCard FigmentOfYourImagination
figmentOfYourImagination =
  agenda (1, A) FigmentOfYourImagination Cards.figmentOfYourImagination (Static 4)

instance RunMessage FigmentOfYourImagination where
  runMessage msg a@(FigmentOfYourImagination attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> FigmentOfYourImagination <$> liftRunMessage msg attrs
