module Arkham.Asset.Cards.FingerprintKit
  ( fingerprintKit
  , FingerprintKit(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype FingerprintKit = FingerprintKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit :: AssetCard FingerprintKit
fingerprintKit = asset FingerprintKit Cards.fingerprintKit

instance HasAbilities FingerprintKit where
  getAbilities (FingerprintKit a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage FingerprintKit where
  runMessage msg a@(FingerprintKit attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [SkillModifier SkillIntellect 1, DiscoveredClues 1]
        , Investigate iid lid (toSource attrs) Nothing SkillIntellect False
        ]
      pure a
    _ -> FingerprintKit <$> runMessage msg attrs
