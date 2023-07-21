module Arkham.Asset.Cards.RiteOfSeeking (
  riteOfSeeking,
  RiteOfSeeking (..),
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

newtype RiteOfSeeking = RiteOfSeeking AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: AssetCard RiteOfSeeking
riteOfSeeking = asset RiteOfSeeking Cards.riteOfSeeking

instance HasAbilities RiteOfSeeking where
  getAbilities (RiteOfSeeking a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Investigate) $
          Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1]
    ]

instance RunMessage RiteOfSeeking where
  runMessage msg a@(RiteOfSeeking attrs) = case msg of
    UseCardAbility iid source@(isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ createCardEffect
            Cards.riteOfSeeking
            Nothing
            source
            (InvestigationTarget iid lid)
        , Investigate
            iid
            lid
            source
            Nothing
            (if skillType == SkillIntellect then SkillWillpower else skillType)
            False
        ]
      pure a
    _ -> RiteOfSeeking <$> runMessage msg attrs
