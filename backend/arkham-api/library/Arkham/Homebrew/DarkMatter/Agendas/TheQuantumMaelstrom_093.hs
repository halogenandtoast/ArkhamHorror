module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom_093 (theQuantumMaelstrom_093) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Classes.HasQueue (pushEnd)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  advanceQuantumMaelstrom,
  getScanResult,
  moveToScannedLocation,
  placeFacedownInThreatArea,
  quantumMaelstromAbilities,
  scanAtYourLocation,
 )

{- | The third of the three printings of agenda 1. They share a front (see
'quantumMaelstromAbilities') and the tail of their back ('advanceQuantumMaelstrom'),
but each back does something different before that tail; this one shuffles the
encounter discard pile back in and then places a face-down encounter card on
every investigator.
-}
newtype TheQuantumMaelstrom_093 = TheQuantumMaelstrom_093 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuantumMaelstrom_093 :: AgendaCard TheQuantumMaelstrom_093
theQuantumMaelstrom_093 = agenda (1, A) TheQuantumMaelstrom_093 Cards.theQuantumMaelstrom_093 (Static 3)

instance HasAbilities TheQuantumMaelstrom_093 where
  getAbilities (TheQuantumMaelstrom_093 a) = quantumMaelstromAbilities a

instance RunMessage TheQuantumMaelstrom_093 where
  runMessage msg a@(TheQuantumMaelstrom_093 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scanAtYourLocation iid (attrs.ability 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws@(getScanResult -> Just _) _ -> do
      pushEnd $ UseCardAbility iid (toSource attrs) 3 ws NoPayment
      pure a
    UseCardAbility iid (isSource attrs -> True) 3 (getScanResult -> Just r) _ -> do
      moveToScannedLocation (attrs.ability 2) iid r
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      eachInvestigator (`placeFacedownInThreatArea` 1)
      advanceQuantumMaelstrom attrs
      pure a
    _ -> TheQuantumMaelstrom_093 <$> liftRunMessage msg attrs
