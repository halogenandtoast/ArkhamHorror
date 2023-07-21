module Arkham.Asset.Cards.StrangeSolution (
  strangeSolution,
  StrangeSolution (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.CampaignLogKey
import Arkham.SkillType

newtype StrangeSolution = StrangeSolution AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strangeSolution :: AssetCard StrangeSolution
strangeSolution = asset StrangeSolution Cards.strangeSolution

instance HasAbilities StrangeSolution where
  getAbilities (StrangeSolution x) =
    [restrictedAbility x 1 ControlsThis $ ActionAbility Nothing $ ActionCost 1]

instance RunMessage StrangeSolution where
  runMessage msg a@(StrangeSolution attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ push
              ( beginSkillTest
                  iid
                  source
                  (InvestigatorTarget iid)
                  SkillIntellect
                  4
              )
    PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          drawing <- drawCards iid attrs 2
          pushAll
            [ Discard (toAbilitySource attrs 1) (toTarget attrs)
            , drawing
            , Record YouHaveIdentifiedTheSolution
            ]
          pure a
    _ -> StrangeSolution <$> runMessage msg attrs
