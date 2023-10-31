module Arkham.Asset.Cards.AncientStone1 (
  ancientStone1,
  AncientStone1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.SkillTest.Base

newtype AncientStone1 = AncientStone1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStone1 :: AssetCard AncientStone1
ancientStone1 = asset AncientStone1 Cards.ancientStone1

instance HasAbilities AncientStone1 where
  getAbilities (AncientStone1 a) = [restrictedAbility a 1 ControlsThis investigateAction_]

instance RunMessage AncientStone1 where
  runMessage msg a@(AncientStone1 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigation <- mkInvestigate iid (toAbilitySource attrs 1) <&> setTarget attrs
      pushAll
        [ skillTestModifiers @LocationId attrs investigation.location [ShroudModifier 3]
        , toMessage investigation
        ]
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ target _ | attrs `is` target -> do
      amount <- min 2 <$> field LocationClues lid
      difficulty <- skillTestDifficulty <$> getJustSkillTest
      shouldRecord <- not <$> getHasRecord YouHaveIdentifiedTheStone
      pushAll
        $ [ InvestigatorDiscoverClues iid lid (toAbilitySource attrs 1) amount (Just Action.Investigate)
          , toDiscardBy iid (toAbilitySource attrs 1) attrs
          ]
        <> [RecordCount YouHaveIdentifiedTheStone difficulty | shouldRecord]
      pure a
    _ -> AncientStone1 <$> runMessage msg attrs
