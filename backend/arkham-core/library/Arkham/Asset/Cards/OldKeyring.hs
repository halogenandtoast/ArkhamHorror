module Arkham.Asset.Cards.OldKeyring (
  oldKeyring,
  OldKeyring (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Projection

newtype OldKeyring = OldKeyring AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldKeyring :: AssetCard OldKeyring
oldKeyring = assetWith OldKeyring Cards.oldKeyring (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities OldKeyring where
  getAbilities (OldKeyring attrs) = [investigateAbility attrs 1 mempty ControlsThis]

instance RunMessage OldKeyring where
  runMessage msg a@(OldKeyring attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- fieldJust InvestigatorLocation iid
      investigation <- mkInvestigate iid (toAbilitySource attrs 1)
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) lid (ShroudModifier (-2))
        , toMessage investigation
        ]
      pure a
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      push $ SpendUses (toTarget attrs) Key 1
      pure a
    _ -> OldKeyring <$> runMessage msg attrs
