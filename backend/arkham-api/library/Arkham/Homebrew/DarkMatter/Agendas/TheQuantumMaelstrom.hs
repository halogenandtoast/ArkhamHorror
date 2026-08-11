module Arkham.Homebrew.DarkMatter.Agendas.TheQuantumMaelstrom (
  theQuantumMaelstrom_091,
  theQuantumMaelstrom_092,
  theQuantumMaelstrom_093,
) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Card.CardDef (CardDef)
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (ScanResult (..), scan, scanAction_, scanEvent)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

{- | All three printings of agenda 1 are mechanically identical; only the
starting station layout on the back differs.

"[action] Scan. Search for the topmost card in the scanning deck with an icon
matching your current location and draw it. If it is a location, put it into
play and move to it. Shuffle the scanning deck."

Drawing a location card already puts it into play (the encounter-draw path
places a drawn location), so ability 2 is the silent hook that performs the
"and move to it" half once the location exists.
-}
newtype TheQuantumMaelstrom = TheQuantumMaelstrom AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mkMaelstrom :: CardDef -> AgendaCard TheQuantumMaelstrom
mkMaelstrom def = agenda (1, A) TheQuantumMaelstrom def (Static 3)

theQuantumMaelstrom_091 :: AgendaCard TheQuantumMaelstrom
theQuantumMaelstrom_091 = mkMaelstrom Cards.theQuantumMaelstrom_091

theQuantumMaelstrom_092 :: AgendaCard TheQuantumMaelstrom
theQuantumMaelstrom_092 = mkMaelstrom Cards.theQuantumMaelstrom_092

theQuantumMaelstrom_093 :: AgendaCard TheQuantumMaelstrom
theQuantumMaelstrom_093 = mkMaelstrom Cards.theQuantumMaelstrom_093

instance HasAbilities TheQuantumMaelstrom where
  getAbilities (TheQuantumMaelstrom a) =
    [ restricted a 1 NoRestriction scanAction_
    , mkAbility a 2 $ SilentForcedAbility $ CampaignEvent #after (Just You) scanEvent
    ]

getScanResult :: [Window] -> Maybe ScanResult
getScanResult = \case
  (windowType -> Window.CampaignEvent key _ v) : _ | key == scanEvent -> Just (toResult v)
  _ : rest -> getScanResult rest
  [] -> Nothing

instance RunMessage TheQuantumMaelstrom where
  runMessage msg a@(TheQuantumMaelstrom attrs) = runQueueT $ case msg of
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
    _ -> TheQuantumMaelstrom <$> liftRunMessage msg attrs
