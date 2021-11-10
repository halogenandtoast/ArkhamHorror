module Arkham.Types.Asset.Cards.RiteOfSeeking
  ( riteOfSeeking
  , RiteOfSeeking(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Id
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype RiteOfSeeking = RiteOfSeeking AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riteOfSeeking :: AssetCard RiteOfSeeking
riteOfSeeking = asset RiteOfSeeking Cards.riteOfSeeking

instance HasAbilities RiteOfSeeking where
  getAbilities (RiteOfSeeking a) =
    [ restrictedAbility
        a
        1
        OwnsThis
        (ActionAbility
          (Just Action.Investigate)
          (Costs [ActionCost 1, UseCost (toId a) Charge 1])
        )
    ]

instance AssetRunner env => RunMessage env RiteOfSeeking where
  runMessage msg a@(RiteOfSeeking attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId @LocationId iid
      a <$ pushAll
        [ CreateEffect "02028" Nothing source (InvestigationTarget iid lid)
        , Investigate iid lid source Nothing SkillWillpower False
        ]
    _ -> RiteOfSeeking <$> runMessage msg attrs
