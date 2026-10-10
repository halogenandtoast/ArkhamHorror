module Arkham.Homebrew.AgesUnwound.Acts.OutOfYourDepth (outOfYourDepth) where

import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Matcher

newtype OutOfYourDepth = OutOfYourDepth ActAttrs
  deriving anyclass (IsAct, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 1. Its only printed text is the reminder "Locations with a resource on
them are 'warded'", which is a reading of the resource count rather than anything
the act has to grant (see
"Arkham.Homebrew.AgesUnwound.Scenarios.TimeRunsOut.Helpers"), so the act is just
its clue requirement: the ordinary group spend the engine offers for an
'actAdvanceCost'.
-}
outOfYourDepth :: ActCard OutOfYourDepth
outOfYourDepth =
  act (1, A) OutOfYourDepth Cards.outOfYourDepth (Just $ GroupClueCost (PerPlayer 3) Anywhere)

-- | Act 1b /Weaving Fate/ is flavour only.
instance RunMessage OutOfYourDepth where
  runMessage msg a@(OutOfYourDepth attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> OutOfYourDepth <$> liftRunMessage msg attrs
