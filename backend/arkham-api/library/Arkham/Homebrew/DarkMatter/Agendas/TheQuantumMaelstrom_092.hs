module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom_092 (theQuantumMaelstrom_092) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card (toCardType)
import Arkham.Card.CardType (CardType (LocationType))
import Arkham.Classes.HasQueue (pushEnd)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  advanceQuantumMaelstrom,
  getScanResult,
  getScanningDeck,
  moveToScannedLocation,
  quantumMaelstromAbilities,
  scanAtYourLocation,
  shuffleEmptyUnstabilizedLocations,
 )
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern Liminal)
import Arkham.Matcher
import Arkham.Message.Lifted.Move

{- | The second of the three printings of agenda 1. They share a front (see
'quantumMaelstromAbilities') and the tail of their back ('advanceQuantumMaelstrom'),
but each back does something different before that tail; this one scans up the
top card of the scanning deck and, if it is a location, drags everyone and every
non-[[Liminal]] enemy onto it.
-}
newtype TheQuantumMaelstrom_092 = TheQuantumMaelstrom_092 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuantumMaelstrom_092 :: AgendaCard TheQuantumMaelstrom_092
theQuantumMaelstrom_092 = agenda (1, A) TheQuantumMaelstrom_092 Cards.theQuantumMaelstrom_092 (Static 3)

instance HasAbilities TheQuantumMaelstrom_092 where
  getAbilities (TheQuantumMaelstrom_092 a) = quantumMaelstromAbilities a

instance RunMessage TheQuantumMaelstrom_092 where
  runMessage msg a@(TheQuantumMaelstrom_092 attrs) = runQueueT $ case msg of
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
      scanningDeck <- getScanningDeck
      case scanningDeck of
        card : rest | toCardType card == LocationType -> do
          push $ SetScenarioDeck ScanningDeck rest
          lid <- placeLocation card
          selectEach UneliminatedInvestigator \iid -> moveTo attrs iid lid
          selectEach (not_ $ EnemyWithTrait Liminal) \eid -> enemyMoveTo attrs eid lid
        _ -> pure ()
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      shuffleEmptyUnstabilizedLocations
      advanceQuantumMaelstrom attrs
      pure a
    _ -> TheQuantumMaelstrom_092 <$> liftRunMessage msg attrs
