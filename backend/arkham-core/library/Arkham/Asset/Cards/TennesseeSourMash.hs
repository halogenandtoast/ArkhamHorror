module Arkham.Asset.Cards.TennesseeSourMash
  ( tennesseeSourMash
  , TennesseeSourMash(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype TennesseeSourMash = TennesseeSourMash AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tennesseeSourMash :: AssetCard TennesseeSourMash
tennesseeSourMash = asset TennesseeSourMash Cards.tennesseeSourMash

instance HasAbilities TennesseeSourMash where
  getAbilities (TennesseeSourMash a) =
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

instance RunMessage TennesseeSourMash where
  runMessage msg a@(TennesseeSourMash attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ skillTestModifier attrs iid (SkillModifier SkillWillpower 2)
      pure a
    InDiscard _ (UseCardAbility iid (isSource attrs -> True) 2 _ _) -> do
      pushAll $ 
        [ skillTestModifier
          attrs
          (InvestigatorTarget iid)
          (SkillModifier SkillCombat 3)
        , ChooseFightEnemy iid (toSource attrs) Nothing SkillCombat mempty False
        ]
      pure a
    _ -> TennesseeSourMash <$> runMessage msg attrs
