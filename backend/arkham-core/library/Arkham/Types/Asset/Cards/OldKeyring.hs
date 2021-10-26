module Arkham.Types.Asset.Cards.OldKeyring
  ( oldKeyring
  , OldKeyring(..)
  ) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Action qualified as Action
import Arkham.Types.Asset.Attrs
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype OldKeyring = OldKeyring AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

oldKeyring :: AssetCard OldKeyring
oldKeyring = handWith OldKeyring Cards.oldKeyring (discardWhenNoUsesL .~ True)

instance HasAbilities OldKeyring where
  getAbilities (OldKeyring attrs) =
    [ restrictedAbility attrs 1 OwnsThis
        $ ActionAbility (Just Action.Investigate)
        $ ActionCost 1
    ]

instance AssetRunner env => RunMessage env OldKeyring where
  runMessage msg a@(OldKeyring attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      lid <- getId iid
      a <$ pushAll
        [ skillTestModifier attrs (LocationTarget lid) (ShroudModifier (-2))
        , Investigate iid lid source Nothing SkillIntellect False
        ]
    PassedSkillTest _ _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> a <$ push (SpendUses (toTarget attrs) Key 1)
    _ -> OldKeyring <$> runMessage msg attrs
