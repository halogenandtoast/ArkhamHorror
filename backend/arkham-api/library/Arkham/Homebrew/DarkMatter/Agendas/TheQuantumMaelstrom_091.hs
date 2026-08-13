module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom_091 (theQuantumMaelstrom_091) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card.CardType (CardType (LocationType))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (
  ScanResult (..),
  getScanResult,
  scan,
  scanAction_,
  scanEventForCardType,
 )
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection

{- | One of the three printings of agenda 1; they are mechanically identical and
only the starting station layout on the back differs.

"[action] Scan. Search for the topmost card in the scanning deck with an icon
matching your current location and draw it. If it is a location, put it into
play and move to it. Shuffle the scanning deck."

Drawing a location card already puts it into play (the encounter-draw path
places a drawn location), so ability 2 is the silent hook that performs the
"and move to it" half once the location exists.
-}
newtype TheQuantumMaelstrom_091 = TheQuantumMaelstrom_091 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theQuantumMaelstrom_091 :: AgendaCard TheQuantumMaelstrom_091
theQuantumMaelstrom_091 = agenda (1, A) TheQuantumMaelstrom_091 Cards.theQuantumMaelstrom_091 (Static 3)

instance HasAbilities TheQuantumMaelstrom_091 where
  getAbilities (TheQuantumMaelstrom_091 a) =
    [ restricted a 1 NoRestriction scanAction_
    , mkAbility a 2
        $ SilentForcedAbility
        $ CampaignEvent #after (Just You) (scanEventForCardType LocationType)
    ]

instance RunMessage TheQuantumMaelstrom_091 where
  runMessage msg a@(TheQuantumMaelstrom_091 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getScanResult -> Just r) _ -> do
      for_ (scannedCard r) \card -> do
        mlid <- selectOne $ LocationWithCardId card.id
        for_ mlid $ moveTo (attrs.ability 2) iid
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      advanceAgendaDeck attrs
      pure a
    _ -> TheQuantumMaelstrom_091 <$> liftRunMessage msg attrs
