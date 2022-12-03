module Arkham.Asset.Cards.TimewornBrand5
  ( timewornBrand5
  , TimewornBrand5(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher hiding ( EnemyDefeated )
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait ( Trait (Elite) )

newtype TimewornBrand5 = TimewornBrand5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timewornBrand5 :: AssetCard TimewornBrand5
timewornBrand5 = asset TimewornBrand5 Cards.timewornBrand5

instance HasAbilities TimewornBrand5 where
  getAbilities (TimewornBrand5 a) =
    [ withTooltip
        "{action} If Timeworn Brand is ready: _Fight_. You get +2 {combat} and deal +1 damage for this attack."
      $ restrictedAbility
          a
          1
          (ControlsThis <> AssetExists (AssetWithId (toId a) <> AssetReady))
      $ ActionAbility Nothing
      $ ActionCost 1
    , withTooltip
        "{action} Exhaust Timeworn Brand: _Fight_. Add your {willpower} to your skill value for this attack. This attack deals +3 damage. If this attack defeats an _Elite_ enemy, draw 3 cards. (Max once per game.)"
      $ limitedAbility (PlayerLimit PerGame 1)
      $ restrictedAbility a 2 ControlsThis
      $ ActionAbility (Just Action.Fight)
      $ ActionCost 1
      <> ExhaustCost (toTarget a)
    ]

instance RunMessage TimewornBrand5 where
  runMessage msg a@(TimewornBrand5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers
          (toSource attrs)
          (InvestigatorTarget iid)
          [SkillModifier SkillCombat 2, DamageDealt 1]
        , ChooseFightEnemy
          iid
          (toAbilitySource attrs 1)
          Nothing
          SkillCombat
          AnyEnemy
          False
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pushAll
        [ skillTestModifiers
          (toSource attrs)
          (InvestigatorTarget iid)
          [AddSkillValue SkillWillpower, DamageDealt 3]
        , ChooseFightEnemy
          iid
          (toAbilitySource attrs 2)
          Nothing
          SkillCombat
          AnyEnemy
          False
        ]
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 2 -> True) traits -> do
      when (Elite `elem` traits) $ for_ (assetController attrs) $ \iid ->
        push $ drawCards iid attrs 3
      pure a
    _ -> TimewornBrand5 <$> runMessage msg attrs
