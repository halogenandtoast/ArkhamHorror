module Arkham.Asset.Cards.OldKeyring
  ( oldKeyring
  , OldKeyring(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Action qualified as Action
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Investigator.Types (Field(..))
import Arkham.Projection
import Arkham.SkillType
import Arkham.Target

newtype OldKeyring = OldKeyring AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldKeyring :: AssetCard OldKeyring
oldKeyring = assetWith OldKeyring Cards.oldKeyring (discardWhenNoUsesL .~ True)

instance HasAbilities OldKeyring where
  getAbilities (OldKeyring attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
    ]

instance RunMessage OldKeyring where
  runMessage msg a@(OldKeyring attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- fieldMap InvestigatorLocation (fromJustNote "must be at a location") iid
      a <$ pushAll
        [ skillTestModifier attrs (LocationTarget lid) (ShroudModifier (-2))
        , Investigate iid lid source Nothing SkillIntellect False
        ]
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (SpendUses (toTarget attrs) Key 1)
    _ -> OldKeyring <$> runMessage msg attrs
