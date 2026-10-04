module Arkham.Homebrew.ConsternationOnTheConstellation.Acts.PlugTheAbyss (plugTheAbyss) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Acts qualified as Cards

newtype PlugTheAbyss = PlugTheAbyss ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 3a, the branch reached by advancing agenda 2. While Luther Marsh has no
remaining health the Tablet of Dagon gains a seal-five-tokens ability that can
destroy it. Objective: advance when the Tablet of Dagon is destroyed.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
plugTheAbyss :: ActCard PlugTheAbyss
plugTheAbyss = act (3, A) PlugTheAbyss Cards.plugTheAbyss Nothing

instance RunMessage PlugTheAbyss where
  runMessage msg (PlugTheAbyss attrs) = runQueueT $ PlugTheAbyss <$> liftRunMessage msg attrs
