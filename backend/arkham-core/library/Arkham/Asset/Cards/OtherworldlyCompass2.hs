module Arkham.Asset.Cards.OtherworldlyCompass2 (
  otherworldlyCompass2,
  OtherworldlyCompass2 (..),
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

newtype OtherworldlyCompass2 = OtherworldlyCompass2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

otherworldlyCompass2 :: AssetCard OtherworldlyCompass2
otherworldlyCompass2 = asset OtherworldlyCompass2 Cards.otherworldlyCompass2

instance HasAbilities OtherworldlyCompass2 where
  getAbilities (OtherworldlyCompass2 a) =
    [ restrictedAbility a 1 ControlsThis $
        ActionAbility (Just Action.Investigate) $
          ActionCost 1
            <> ExhaustCost (toTarget a)
    ]

instance RunMessage OtherworldlyCompass2 where
  runMessage msg a@(OtherworldlyCompass2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      lid <- getJustLocation iid
      revealedLocations <-
        selectCount $
          RevealedLocation
            <> ConnectedTo
              (locationWithInvestigator iid)
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ skillTestModifier
            attrs
            (LocationTarget lid)
            (ShroudModifier (-revealedLocations))
        , Investigate iid lid (toSource attrs) Nothing skillType False
        ]
      pure a
    _ -> OtherworldlyCompass2 <$> runMessage msg attrs
