module Arkham.Act.Cards.FindingAWayInside (findingAWayInside) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Query
import Arkham.Message.Lifted.Choose

newtype FindingAWayInside = FindingAWayInside ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

findingAWayInside :: ActCard FindingAWayInside
findingAWayInside = act (1, A) FindingAWayInside Cards.findingAWayInside (groupClueCost $ Static 2)

instance RunMessage FindingAWayInside where
  runMessage msg a@(FindingAWayInside attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) (isSource attrs -> True) AdvancedWithClues -> do
      lead <- getLead
      investigators <- getInvestigators
      adamLynch <- fetchCard Assets.adamLynch
      chooseTargetM lead investigators (`takeControlOfSetAsideAsset` adamLynch)
      revealMatching "Museum Halls"
      advanceToAct attrs Acts.nightAtTheMuseum A
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      revealMatching "Museum Halls"
      advanceToAct attrs Acts.breakingAndEntering A
      pure a
    _ -> FindingAWayInside <$> liftRunMessage msg attrs
