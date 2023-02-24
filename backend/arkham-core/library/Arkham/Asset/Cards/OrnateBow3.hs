module Arkham.Asset.Cards.OrnateBow3
  ( ornateBow3
  , OrnateBow3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType

newtype OrnateBow3 = OrnateBow3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ornateBow3 :: AssetCard OrnateBow3
ornateBow3 = asset OrnateBow3 Cards.ornateBow3

instance HasAbilities OrnateBow3 where
  getAbilities (OrnateBow3 a) =
    [ restrictedAbility a 1 ControlsThis
      $ ActionAbilityWithSkill (Just Action.Fight) SkillAgility
      $ ActionCost 1
      <> UseCost (AssetWithId $ toId a) Ammo 1
    , restrictedAbility
        a
        2
        (ControlsThis
        <> AssetExists (AssetWithId (toId a) <> NotAsset (AssetWithUses Ammo))
        )
      $ ActionAbility Nothing
      $ ActionCost 1
    ]

instance RunMessage OrnateBow3 where
  runMessage msg a@(OrnateBow3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
          attrs
          (InvestigatorTarget iid)
          [DamageDealt 2, SkillModifier SkillAgility 2]
        , ChooseFightEnemy
          iid
          (toSource attrs)
          Nothing
          SkillAgility
          mempty
          False
        ]
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AddUses (toId attrs) Ammo 1
      pure a
    _ -> OrnateBow3 <$> runMessage msg attrs
