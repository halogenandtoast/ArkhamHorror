module Arkham.Homebrew.ConsternationOnTheConstellation.Agendas.SummonThoseBelow (summonThoseBelow) where

import Arkham.Agenda.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Agendas qualified as Cards

newtype SummonThoseBelow = SummonThoseBelow AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Agenda 3a, the branch reached by advancing agenda 2. Exhausted locations do not
ready during upkeep, and each Cultist enemy gets +1 fight and +1 evade.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
summonThoseBelow :: AgendaCard SummonThoseBelow
summonThoseBelow = agenda (3, A) SummonThoseBelow Cards.summonThoseBelow (Static 2)

instance RunMessage SummonThoseBelow where
  runMessage msg (SummonThoseBelow attrs) = runQueueT $ SummonThoseBelow <$> liftRunMessage msg attrs
