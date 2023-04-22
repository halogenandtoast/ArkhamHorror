module Arkham.Asset.Cards.RiteOfSeeking4
  ( riteOfSeeking4
  , RiteOfSeeking4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype RiteOfSeeking4 = RiteOfSeeking4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking4 :: AssetCard RiteOfSeeking4
riteOfSeeking4 = asset RiteOfSeeking4 Cards.riteOfSeeking4

instance HasAbilities RiteOfSeeking4 where
  getAbilities (RiteOfSeeking4 a) =
    [ restrictedAbility a 1 ControlsThis $ ActionAbility
        (Just Action.Investigate)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage RiteOfSeeking4 where
  runMessage msg a@(RiteOfSeeking4 attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ CreateEffect "02233" Nothing source (InvestigationTarget iid lid) -- same effect as base
        , skillTestModifier
          source
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 2)
        , Investigate iid lid source Nothing (if skillType == SkillIntellect then SkillWillpower else skillType) False
        ]
      pure a
    _ -> RiteOfSeeking4 <$> runMessage msg attrs
