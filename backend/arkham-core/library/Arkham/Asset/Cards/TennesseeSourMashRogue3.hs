module Arkham.Asset.Cards.TennesseeSourMashRogue3
  ( tennesseeSourMashRogue3
  , TennesseeSourMashRogue3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype TennesseeSourMashRogue3 = TennesseeSourMashRogue3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tennesseeSourMashRogue3 :: AssetCard TennesseeSourMashRogue3
tennesseeSourMashRogue3 = asset TennesseeSourMashRogue3 Cards.tennesseeSourMashRogue3

instance HasAbilities TennesseeSourMashRogue3 where
  getAbilities (TennesseeSourMashRogue3 a) =
    [ restrictedAbility
        a
        1
        (ControlsThis <> DuringSkillTest (SkillTestOnTreachery AnyTreachery))
      $ FastAbility
      $ ExhaustCost (toTarget a)
      <> UseCost (AssetWithId $ toId a) Supply 1
    , restrictedAbility a 2 ControlsThis
      $ ActionAbility (Just Action.Fight)
      $ ActionCost 1
      <> DiscardCost FromPlay (toTarget a)
    ]

instance RunMessage TennesseeSourMashRogue3 where
  runMessage msg a@(TennesseeSourMashRogue3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier attrs iid (SkillModifier SkillWillpower 3)
      pure a
    InDiscard _ (UseCardAbility iid (isSource attrs -> True) 2 _ _) -> do
      pushAll $ 
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 1, SkillModifier SkillCombat 3]
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    _ -> TennesseeSourMashRogue3 <$> runMessage msg attrs
