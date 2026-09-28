module Arkham.Homebrew.CircusExMortis.Agendas.LackOfRestraint (lackOfRestraint) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.CircusExMortis.Agendas.IntoTheLionsDen (
  heatOfTheMoment,
  heatOfTheMomentFailure,
 )
import Arkham.Homebrew.CircusExMortis.CardDefs.Agendas qualified as Cards

newtype LackOfRestraint = LackOfRestraint AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lackOfRestraint :: AgendaCard LackOfRestraint
lackOfRestraint = agenda (2, A) LackOfRestraint Cards.lackOfRestraint (Static 6)

instance RunMessage LackOfRestraint where
  runMessage msg a@(LackOfRestraint attrs) = runQueueT $ case msg of
    -- "Popular Demand" is identical to agenda 1's "Heat of the Moment"
    AdvanceAgenda (isSide B attrs -> True) -> do
      heatOfTheMoment attrs
      advanceAgendaDeckAfterSkillTest attrs
      pure a
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      heatOfTheMomentFailure attrs iid
      pure a
    _ -> LackOfRestraint <$> liftRunMessage msg attrs
