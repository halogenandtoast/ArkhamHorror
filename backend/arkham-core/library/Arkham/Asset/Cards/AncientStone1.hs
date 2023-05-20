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
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection
import Arkham.SkillTest.Base

newtype AncientStone1 = AncientStone1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStone1 :: AssetCard AncientStone1
ancientStone1 = asset AncientStone1 Cards.ancientStone1

instance HasAbilities AncientStone1 where
  getAbilities (AncientStone1 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Investigate) (ActionCost 1)
    ]

instance RunMessage AncientStone1 where
  runMessage msg a@(AncientStone1 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      lid <-
        fieldMap
          InvestigatorLocation
          (fromJustNote "must be at a location")
          iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ skillTestModifiers attrs (LocationTarget lid) [ShroudModifier 3]
        , Investigate iid lid source (Just $ toTarget attrs) skillType False
        ]
      pure a
    Successful (Action.Investigate, LocationTarget lid) iid _ target _
      | isTarget attrs target -> do
          clueCount <- field LocationClues lid
          let amount = min clueCount 2
          difficulty <-
            skillTestDifficulty . fromJustNote "no skill test" <$> getSkillTest
          shouldRecord <- not <$> getHasRecord YouHaveIdentifiedTheStone
          pushAll $
            [ InvestigatorDiscoverClues iid lid (toAbilitySource attrs 1) amount (Just Action.Investigate)
            , Discard (toAbilitySource attrs 1) (toTarget attrs)
            ]
              <> [RecordCount YouHaveIdentifiedTheStone difficulty | shouldRecord]
          pure a
    _ -> AncientStone1 <$> runMessage msg attrs
