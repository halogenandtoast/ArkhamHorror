module Arkham.Asset.Cards.FingerprintKit4 (
  fingerprintKit4,
  FingerprintKit4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype FingerprintKit4 = FingerprintKit4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit4 :: AssetCard FingerprintKit4
fingerprintKit4 = asset FingerprintKit4 Cards.fingerprintKit4

instance HasAbilities FingerprintKit4 where
  getAbilities (FingerprintKit4 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage FingerprintKit4 where
  runMessage msg a@(FingerprintKit4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      lid <-
        fieldMap
          InvestigatorLocation
          (fromJustNote "must be at a location")
          iid
      pushAll
        [ skillTestModifiers
            attrs
            (InvestigatorTarget iid)
            [SkillModifier SkillIntellect 2, DiscoveredClues 2]
        , Investigate iid lid (toSource attrs) Nothing SkillIntellect False
        ]
      pure a
    _ -> FingerprintKit4 <$> runMessage msg attrs
