module Arkham.Asset.Cards.EnchantedBladeGuardian3 (
  enchantedBladeGuardian3,
  EnchantedBladeGuardian3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Metadata = Metadata {empowered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EnchantedBladeGuardian3 = EnchantedBladeGuardian3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBladeGuardian3 :: AssetCard EnchantedBladeGuardian3
enchantedBladeGuardian3 = asset (EnchantedBladeGuardian3 . (`with` Metadata False)) Cards.enchantedBladeGuardian3

instance HasAbilities EnchantedBladeGuardian3 where
  getAbilities (EnchantedBladeGuardian3 (attrs `With` _)) =
    [restrictedAbility attrs 1 ControlsThis $ ActionAbility ([Action.Fight]) (ActionCost 1)]

instance RunMessage EnchantedBladeGuardian3 where
  runMessage msg a@(EnchantedBladeGuardian3 (attrs `With` meta)) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushAll
        [ skillTestModifier (toAbilitySource attrs 1) iid (SkillModifier #combat 2)
        , chooseFightEnemy iid (toAbilitySource attrs 1) #combat
        ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      player <- getPlayer iid
      pushWhen (useCount (assetUses attrs) > 0)
        $ chooseOne player
        $ [ Label "Spend 1 use to empower" [DoStep 1 (SpendUses (toTarget attrs) Charge 1)]
          , Label "Do not spend use" []
          ]
      pure a
    DoStep 1 msg'@(SpendUses (isTarget attrs -> True) _ _) -> do
      for_ (assetController attrs)
        $ \iid -> pushAll [msg', skillTestModifier (toAbilitySource attrs 1) iid (DamageDealt 1)]
      pure . EnchantedBladeGuardian3 $ attrs `with` Metadata True
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ | empowered meta -> do
      for_ (assetController attrs) $ \iid -> do
        drawing <- drawCards iid (toAbilitySource attrs 1) 1
        pushAll [drawing, HealHorror (toTarget iid) (toAbilitySource attrs 1) 1]
      pure a
    SkillTestEnds _ _ -> pure . EnchantedBladeGuardian3 $ attrs `with` Metadata False
    _ -> EnchantedBladeGuardian3 . (`with` meta) <$> runMessage msg attrs
