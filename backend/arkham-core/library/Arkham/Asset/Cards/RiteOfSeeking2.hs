module Arkham.Asset.Cards.RiteOfSeeking2 (
  riteOfSeeking2,
  RiteOfSeeking2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype RiteOfSeeking2 = RiteOfSeeking2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking2 :: AssetCard RiteOfSeeking2
riteOfSeeking2 = asset RiteOfSeeking2 Cards.riteOfSeeking2

instance HasAbilities RiteOfSeeking2 where
  getAbilities (RiteOfSeeking2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          (Just Action.Investigate)
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage RiteOfSeeking2 where
  runMessage msg a@(RiteOfSeeking2 attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ CreateEffect "02233" Nothing source (InvestigationTarget iid lid) -- same effect as base
        , skillTestModifier
            source
            (InvestigatorTarget iid)
            (SkillModifier SkillWillpower 2)
        , Investigate
            iid
            lid
            source
            Nothing
            (if skillType == SkillIntellect then SkillWillpower else skillType)
            False
        ]
      pure a
    _ -> RiteOfSeeking2 <$> runMessage msg attrs
