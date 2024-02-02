module Arkham.Asset.Cards.TimewornBrand5 (
  timewornBrand5,
  TimewornBrand5 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Trait (Trait (Elite))

newtype TimewornBrand5 = TimewornBrand5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

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
        $ ActionAbility [] (ActionCost 1)
    , withTooltip
        "{action} Exhaust Timeworn Brand: _Fight_. Add your {willpower} to your skill value for this attack. This attack deals +3 damage. If this attack defeats an _Elite_ enemy, draw 3 cards. (Max once per game.)"
        $ limitedAbility (MaxPer Cards.timewornBrand5 PerGame 1)
        $ restrictedAbility a 2 ControlsThis
        $ ActionAbility ([Action.Fight]) (ActionCost 1 <> exhaust a)
    ]

instance RunMessage TimewornBrand5 where
  runMessage msg a@(TimewornBrand5 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifiers (toAbilitySource attrs 1) iid [SkillModifier #combat 2, DamageDealt 1]
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      pushAll
        [ skillTestModifiers (toAbilitySource attrs 2) iid [AddSkillValue #willpower, DamageDealt 3]
        , chooseFightEnemy iid (toAbilitySource attrs 2) #combat
        ]
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 2 -> True) traits | Elite `elem` traits -> do
      for_ (assetController attrs) $ \iid -> do
        pushM $ drawCards iid (toAbilitySource attrs 2) 3
      pure a
    _ -> TimewornBrand5 <$> runMessage msg attrs
