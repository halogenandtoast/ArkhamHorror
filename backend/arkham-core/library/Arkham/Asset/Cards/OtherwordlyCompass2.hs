module Arkham.Asset.Cards.OtherwordlyCompass2
  ( otherwordlyCompass2
  , OtherwordlyCompass2(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target

newtype OtherwordlyCompass2 = OtherwordlyCompass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherwordlyCompass2 :: AssetCard OtherwordlyCompass2
otherwordlyCompass2 = asset OtherwordlyCompass2 Cards.otherwordlyCompass2

instance HasAbilities OtherwordlyCompass2 where
  getAbilities (OtherwordlyCompass2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
    ]

instance RunMessage OtherwordlyCompass2 where
  runMessage msg a@(OtherwordlyCompass2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      revealedLocations <- selectCount $ RevealedLocation <> ConnectedTo
        (locationWithInvestigator iid)
      pushAll
        [ skillTestModifier
          attrs
          (LocationTarget lid)
          (ShroudModifier (-revealedLocations))
        , Investigate iid lid (toSource attrs) Nothing SkillIntellect False
        ]
      pure a
    _ -> OtherwordlyCompass2 <$> runMessage msg attrs
