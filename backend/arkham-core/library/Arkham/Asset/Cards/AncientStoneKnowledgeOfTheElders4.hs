module Arkham.Asset.Cards.AncientStoneKnowledgeOfTheElders4
  ( ancientStoneKnowledgeOfTheElders4
  , AncientStoneKnowledgeOfTheElders4(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey

newtype AncientStoneKnowledgeOfTheElders4 = AncientStoneKnowledgeOfTheElders4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStoneKnowledgeOfTheElders4
  :: AssetCard AncientStoneKnowledgeOfTheElders4
ancientStoneKnowledgeOfTheElders4 = asset
  AncientStoneKnowledgeOfTheElders4
  Cards.ancientStoneKnowledgeOfTheElders4

instance RunMessage AncientStoneKnowledgeOfTheElders4 where
  runMessage msg a@(AncientStoneKnowledgeOfTheElders4 attrs) = case msg of
    InvestigatorPlayedAsset _ aid | aid == toId attrs -> do
      n <- getRecordCount YouHaveIdentifiedTheStone
      push $ AddUses (toTarget attrs) Secret n
      pure a
    _ -> AncientStoneKnowledgeOfTheElders4 <$> runMessage msg attrs
