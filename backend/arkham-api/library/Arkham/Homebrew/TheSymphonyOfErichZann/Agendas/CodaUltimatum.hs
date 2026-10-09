{- | Coda Ultimatum, printed on the back of agenda 3, Opus Magnum.

It is its own agenda here rather than Opus Magnum's b side, because what this
card does is /stay/: it becomes both the current act and the current agenda and
the scenario runs on until every undefeated investigator has resigned. Two
engine behaviours are keyed to a flipped agenda being on its way out -- mythos
doom only lands on an unflipped agenda, and the doom pool is not drawn for one --
and the Window to Nothingness watches for that mythos doom.

It prints no doom value, so it never advances.
-}
module Arkham.Homebrew.TheSymphonyOfErichZann.Agendas.CodaUltimatum (codaUltimatum) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Agendas qualified as Cards
import Arkham.Matcher

newtype CodaUltimatum = CodaUltimatum AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

codaUltimatum :: AgendaCard CodaUltimatum
codaUltimatum =
  agendaWith (4, A) CodaUltimatum Cards.codaUltimatum (Static 0) (doomThresholdL .~ Nothing)

{- | "Objective - Save yourself! If each undefeated investigator has resigned,
(→R1)."

This card stands in for the act as well as the agenda, so the objective is
offered here rather than left to the engine's own "everyone is eliminated" path,
which would end the scenario out from under the card instead of through it.
-}
instance HasAbilities CodaUltimatum where
  getAbilities (CodaUltimatum a) =
    [onlyOnce $ restricted a 1 AllUndefeatedInvestigatorsResigned $ Objective $ forced AnyWindow]

instance RunMessage CodaUltimatum where
  runMessage msg a@(CodaUltimatum attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push R1
      pure a
    _ -> CodaUltimatum <$> liftRunMessage msg attrs
