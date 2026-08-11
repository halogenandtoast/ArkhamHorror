module Arkham.Homebrew.DarkMatter.Agendas.TheGhostShip (theGhostShip) where

import Arkham.Ability
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Homebrew.DarkMatter.CardDefs.Agendas qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Enemies
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.DarkMatter.Helpers (scan, scanAction_)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Projection

newtype TheGhostShip = TheGhostShip AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGhostShip :: AgendaCard TheGhostShip
theGhostShip = agenda (2, A) TheGhostShip Cards.theGhostShip (Static 7)

-- "[action] If there are no clues on your current location: Scan."
instance HasAbilities TheGhostShip where
  getAbilities (TheGhostShip a) =
    [restricted a 1 (exists $ YourLocation <> LocationWithoutClues) scanAction_]

{- | This agenda has no printed back: the reverse of the physical card is the
UPL-A21 'Demhe' enemy. Advancing flips it into play at the Cargo Hold and
continues on to agenda 3a.
-}
instance RunMessage TheGhostShip where
  runMessage msg a@(TheGhostShip attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      cargoHold <- selectJust $ locationIs Locations.cargoHold
      createEnemyAt_ Enemies.uplA21Demhe cargoHold
      advanceAgendaDeck attrs
      pure a
    _ -> TheGhostShip <$> liftRunMessage msg attrs
