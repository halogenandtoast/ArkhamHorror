module Arkham.Homebrew.AgesUnwound.Agendas.BreakingPoint (breakingPoint) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Agendas qualified as Cards

newtype BreakingPoint = BreakingPoint AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingPoint :: AgendaCard BreakingPoint
breakingPoint = agenda (4, A) BreakingPoint Cards.breakingPoint (Static 5)

instance RunMessage BreakingPoint where
  runMessage msg a@(BreakingPoint attrs) = runQueueT $ case msg of
    {- "No More. Each remaining investigator is defeated."

    Nothing is recorded here: with no investigator left the scenario reaches no
    resolution, and the scenario's `NoResolution` branch routes to Investigator
    Defeat and then Resolution 1, which is what the guide asks for. -}
    AdvanceAgenda (isSide B attrs -> True) -> do
      eachInvestigator (investigatorDefeated attrs)
      pure a
    _ -> BreakingPoint <$> liftRunMessage msg attrs
