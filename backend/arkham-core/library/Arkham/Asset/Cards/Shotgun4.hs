module Arkham.Asset.Cards.Shotgun4 (
  Shotgun4 (..),
  shotgun4,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Window
import Arkham.EffectMetadata

newtype Shotgun4 = Shotgun4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shotgun4 :: AssetCard Shotgun4
shotgun4 = asset Shotgun4 Cards.shotgun4

instance HasAbilities Shotgun4 where
  getAbilities (Shotgun4 a) = [fightAbility a 1 (assetUseCost a Ammo 1) ControlsThis]

instance RunMessage Shotgun4 where
  runMessage msg a@(Shotgun4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #combat 3)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      -- This has to be handled specially for cards like Oops!
      let val = max 1 (min 5 n)
      push
        $ CreateWindowModifierEffect
          EffectSkillTestWindow
          (FailedByEffectModifiers $ toModifiers (toAbilitySource attrs 1) [DamageDealt val])
          (toAbilitySource attrs 1)
          (toTarget iid)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      push $ skillTestModifier (toAbilitySource attrs 1) iid (DamageDealt $ max 1 (min 5 n))
      pure a
    _ -> Shotgun4 <$> runMessage msg attrs
