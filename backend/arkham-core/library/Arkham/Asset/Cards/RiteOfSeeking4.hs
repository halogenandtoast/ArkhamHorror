module Arkham.Asset.Cards.RiteOfSeeking4
  ( riteOfSeeking4
  , RiteOfSeeking4(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Id
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Target

newtype RiteOfSeeking4 = RiteOfSeeking4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking4 :: AssetCard RiteOfSeeking4
riteOfSeeking4 = asset RiteOfSeeking4 Cards.riteOfSeeking4

instance HasAbilities RiteOfSeeking4 where
  getAbilities (RiteOfSeeking4 a) =
    [ restrictedAbility a 1 OwnsThis $ ActionAbility
        (Just Action.Investigate)
        (Costs [ActionCost 1, UseCost (toId a) Charge 1])
    ]

instance AssetRunner env => RunMessage env RiteOfSeeking4 where
  runMessage msg a@(RiteOfSeeking4 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      a <$ pushAll
        [ CreateEffect "02233" Nothing source (InvestigationTarget iid lid) -- same effect as base
        , skillTestModifier
          source
          (InvestigatorTarget iid)
          (SkillModifier SkillWillpower 2)
        , Investigate iid lid source Nothing SkillWillpower False
        ]
    _ -> RiteOfSeeking4 <$> runMessage msg attrs
