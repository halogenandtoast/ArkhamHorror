module Arkham.Homebrew.ConsternationOnTheConstellation.Agendas.SearchTheShip (searchTheShip) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Agendas qualified as Cards

newtype SearchTheShip = SearchTheShip AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Agenda 2a. Forced at the end of the enemy phase: place 1 doom on each Crate of
Goods for each ready Cultist enemy at its location. Advancing hands the tablet to
the cult -- Luther Marsh spawns at the Bridge holding it -- and sets up the
"Summon Those Below" / "Plug the Abyss" branch.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
searchTheShip :: AgendaCard SearchTheShip
searchTheShip = agenda (2, A) SearchTheShip Cards.searchTheShip (Static 12)

instance RunMessage SearchTheShip where
  runMessage msg (SearchTheShip attrs) = runQueueT $ SearchTheShip <$> liftRunMessage msg attrs
