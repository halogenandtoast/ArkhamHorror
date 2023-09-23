module Arkham.Asset.Cards.Flashlight (
  Flashlight (..),
  flashlight,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype Flashlight = Flashlight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight :: AssetCard Flashlight
flashlight = asset Flashlight Cards.flashlight

instance HasAbilities Flashlight where
  getAbilities (Flashlight x) =
    [restrictedAbility x 1 ControlsThis $ investigateAction (assetUseCost x Supply 1)]

instance RunMessage Flashlight where
  runMessage msg a@(Flashlight attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust InvestigatorLocation iid
      skillType <- field LocationInvestigateSkill lid
      pushAll
        [ skillTestModifier attrs lid (ShroudModifier (-2))
        , Investigate iid lid (toSource attrs) Nothing skillType False
        ]
      pure a
    _ -> Flashlight <$> runMessage msg attrs
