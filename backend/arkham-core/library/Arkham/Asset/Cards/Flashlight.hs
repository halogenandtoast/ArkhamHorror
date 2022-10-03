module Arkham.Asset.Cards.Flashlight
  ( Flashlight(..)
  , flashlight
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

newtype Flashlight = Flashlight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight :: AssetCard Flashlight
flashlight = asset Flashlight Cards.flashlight

instance HasAbilities Flashlight where
  getAbilities (Flashlight x) =
    [ restrictedAbility x 1 ControlsThis $ ActionAbility
        (Just Action.Investigate)
        (Costs [ActionCost 1, UseCost (AssetWithId $ toId x) Supply 1])
    ]

instance RunMessage Flashlight where
  runMessage msg a@(Flashlight attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      lid <- fieldMap
        InvestigatorLocation
        (fromJustNote "must be at a location")
        iid
      a <$ pushAll
        [ skillTestModifier attrs (LocationTarget lid) (ShroudModifier (-2))
        , Investigate iid lid source Nothing SkillIntellect False
        ]
    _ -> Flashlight <$> runMessage msg attrs
