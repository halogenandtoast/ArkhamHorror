module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom_091 (theQuantumMaelstrom_091) where

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
  shuffleEmptyUnstabilizedLocations,
 )
import Arkham.Matcher
import Arkham.Token qualified as Token

{- | The first of the three printings of agenda 1. They share a front (see
'quantumMaelstromAbilities') and the tail of their back ('advanceQuantumMaelstrom'),
but each back does something different before that tail; this one places a
face-down encounter card on every investigator standing on an unstabilized
location and returns the empty unstabilized locations to the scanning deck.
-}
newtype TheQuantumMaelstrom_091 = TheQuantumMaelstrom_091 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuantumMaelstrom_091 :: AgendaCard TheQuantumMaelstrom_091
theQuantumMaelstrom_091 = agenda (1, A) TheQuantumMaelstrom_091 Cards.theQuantumMaelstrom_091 (Static 3)

instance HasAbilities TheQuantumMaelstrom_091 where
  getAbilities (TheQuantumMaelstrom_091 a) = quantumMaelstromAbilities a

instance RunMessage TheQuantumMaelstrom_091 where
  runMessage msg a@(TheQuantumMaelstrom_091 attrs) = runQueueT $ case msg of
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
      investigators <- select $ InvestigatorAt $ not_ $ LocationWithToken Token.Resource
      for_ investigators (`placeFacedownInThreatArea` 1)
      shuffleEmptyUnstabilizedLocations
      advanceQuantumMaelstrom attrs
      pure a
    _ -> TheQuantumMaelstrom_091 <$> liftRunMessage msg attrs
