module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom_092 (theQuantumMaelstrom_092) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card (toCardType)
import Arkham.Card.CardType (CardType (LocationType))
import Arkham.Classes.HasQueue (pushEnd)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  ScanResult (..),
  addImpendingDoom,
  getScanResult,
  getScanningDeck,
  scan,
  scanAction_,
  scanEventForCardType,
  shuffleEmptyUnstabilizedLocations,
 )
import Arkham.Homebrew.DarkMatter.ScenarioDeckKeys (pattern ScanningDeck)
import Arkham.Homebrew.DarkMatter.Traits (pattern Liminal)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection

{- | One of the three printings of agenda 1; they are mechanically identical and
only the starting station layout on the back differs.

"[action] Scan. Search for the topmost card in the scanning deck with an icon
matching your current location and draw it. If it is a location, put it into
play and move to it. Shuffle the scanning deck."

Ability 2 defers the move until the existing scanned-card draw has had a chance
to put the location into play. Ability 3 then moves the scanning investigator,
placing the location itself only as a fallback.
-}
newtype TheQuantumMaelstrom_092 = TheQuantumMaelstrom_092 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuantumMaelstrom_092 :: AgendaCard TheQuantumMaelstrom_092
theQuantumMaelstrom_092 = agenda (1, A) TheQuantumMaelstrom_092 Cards.theQuantumMaelstrom_092 (Static 3)

instance HasAbilities TheQuantumMaelstrom_092 where
  getAbilities (TheQuantumMaelstrom_092 a) =
    [ restricted a 1 NoRestriction scanAction_
    , mkAbility a 2
        $ SilentForcedAbility
        $ CampaignEvent #after (Just You) (scanEventForCardType LocationType)
    ]

instance RunMessage TheQuantumMaelstrom_092 where
  runMessage msg a@(TheQuantumMaelstrom_092 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 ws@(getScanResult -> Just _) _ -> do
      pushEnd $ UseCardAbility iid (toSource attrs) 3 ws NoPayment
      pure a
    UseCardAbility iid (isSource attrs -> True) 3 (getScanResult -> Just r) _ -> do
      for_ (scannedCard r) \card -> do
        lid <- selectOne (LocationWithCardId card.id) >>= maybe (placeLocation card) pure
        moveTo (attrs.ability 2) iid lid
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
      agendas <- selectCount AnyAgenda
      if agendas > 1
        then advanceAgendaDeck attrs
        else do
          addImpendingDoom 1
          push $ ResetAgendaDeckToStage 1
      pure a
    _ -> TheQuantumMaelstrom_092 <$> liftRunMessage msg attrs
