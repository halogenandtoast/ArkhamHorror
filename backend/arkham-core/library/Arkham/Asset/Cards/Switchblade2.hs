module Arkham.Asset.Cards.Switchblade2 (
  Switchblade2 (..),
  switchblade2,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype Switchblade2 = Switchblade2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

switchblade2 :: AssetCard Switchblade2
switchblade2 = asset Switchblade2 Cards.switchblade2

instance HasAbilities Switchblade2 where
  getAbilities (Switchblade2 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility ([Action.Fight]) (ActionCost 1)
    ]

instance RunMessage Switchblade2 where
  runMessage msg a@(Switchblade2 attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifier
                  attrs
                  (InvestigatorTarget iid)
                  (SkillModifier SkillCombat 2)
              , ChooseFightEnemy iid source Nothing SkillCombat mempty False
              ]
    PassedSkillTest iid (Just Action.Fight) source SkillTestInitiatorTarget {} _ n
      | n >= 2 && isSource attrs source ->
          a
            <$ push
              (skillTestModifier attrs (InvestigatorTarget iid) (DamageDealt 1))
    _ -> Switchblade2 <$> runMessage msg attrs
