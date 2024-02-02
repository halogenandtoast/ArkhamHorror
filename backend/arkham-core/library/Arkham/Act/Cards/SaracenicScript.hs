module Arkham.Act.Cards.SaracenicScript (
  SaracenicScript (..),
  saracenicScript,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Classes
import Arkham.Matcher

newtype SaracenicScript = SaracenicScript ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData, HasAbilities)

saracenicScript :: ActCard SaracenicScript
saracenicScript =
  act
    (1, A)
    SaracenicScript
    Cards.saracenicScript
    (Just $ GroupClueCost (PerPlayer 2) (LocationWithTitle "Whateley Ruins"))

instance RunMessage SaracenicScript where
  runMessage msg a@(SaracenicScript attrs@ActAttrs {..}) = case msg of
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      survived <- getHasRecord DrHenryArmitageSurvivedTheDunwichLegacy
      investigatorIds <- getInvestigatorIds
      esotericFormulas <- getSetAsideCardsMatching $ cardIs Assets.esotericFormula
      pushAll
        $ zipWith TakeControlOfSetAsideAsset investigatorIds esotericFormulas
        <> [PlaceDoomOnAgenda | not survived]
        <> [advanceActDeck attrs]
      pure a
    _ -> SaracenicScript <$> runMessage msg attrs
