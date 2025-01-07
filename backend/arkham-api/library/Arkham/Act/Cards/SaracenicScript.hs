module Arkham.Act.Cards.SaracenicScript (saracenicScript) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Matcher
import Arkham.Helpers.Log (unlessHasRecord)
import Arkham.Helpers.Query (getSetAsideCardsMatching, getInvestigators)

newtype SaracenicScript = SaracenicScript ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

saracenicScript :: ActCard SaracenicScript
saracenicScript =
  act
    (1, A)
    SaracenicScript
    Cards.saracenicScript
    (Just $ GroupClueCost (PerPlayer 2) "Whateley Ruins")

instance RunMessage SaracenicScript where
  runMessage msg a@(SaracenicScript attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      investigators <- getInvestigators
      esotericFormulas <- getSetAsideCardsMatching $ cardIs Assets.esotericFormula
      zipWithM_ takeControlOfSetAsideAsset investigators esotericFormulas

      unlessHasRecord DrHenryArmitageSurvivedTheDunwichLegacy $ placeDoomOnAgenda 1
      advanceActDeck attrs
      pure a
    _ -> SaracenicScript <$> liftRunMessage msg attrs
