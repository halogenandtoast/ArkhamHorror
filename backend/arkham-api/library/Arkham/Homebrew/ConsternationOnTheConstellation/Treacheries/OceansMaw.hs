module Arkham.Homebrew.ConsternationOnTheConstellation.Treacheries.OceansMaw (oceansMaw) where

import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OceansMaw = OceansMaw TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Test [intellect] (3); on a failure take 1 damage. Then attach an Ally asset you
control to Open Water, where any investigator at Open Water may reclaim it. With
no Ally assets you are moved to Open Water instead.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
oceansMaw :: TreacheryCard OceansMaw
oceansMaw = treachery OceansMaw Cards.oceansMaw

instance RunMessage OceansMaw where
  runMessage msg (OceansMaw attrs) = runQueueT $ OceansMaw <$> liftRunMessage msg attrs
