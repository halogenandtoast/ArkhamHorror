module Arkham.Asset.Cards.EnchantedBladeGuardian3 (enchantedBladeGuardian3, EnchantedBladeGuardian3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Id
import Arkham.Prelude

newtype Metadata = Metadata {empowered :: Maybe SkillTestId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EnchantedBladeGuardian3 = EnchantedBladeGuardian3 (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enchantedBladeGuardian3 :: AssetCard EnchantedBladeGuardian3
enchantedBladeGuardian3 = asset (EnchantedBladeGuardian3 . (`with` Metadata Nothing)) Cards.enchantedBladeGuardian3

instance HasAbilities EnchantedBladeGuardian3 where
  getAbilities (EnchantedBladeGuardian3 (attrs `With` _)) =
    [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage EnchantedBladeGuardian3 where
  runMessage msg a@(EnchantedBladeGuardian3 (attrs `With` meta)) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      pushAll [skillTestModifier sid source iid (SkillModifier #combat 2), chooseFight]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      player <- getPlayer iid
      pushWhen (findWithDefault 0 Charge (assetUses attrs) > 0)
        $ chooseOne
          player
          [ Label "Spend 1 use to empower" [DoStep 1 (SpendUses (attrs.ability 1) (toTarget attrs) Charge 1)]
          , Label "Do not spend use" []
          ]
      pure a
    DoStep 1 msg'@(SpendUses _ (isTarget attrs -> True) _ _) -> do
      getSkillTestId >>= \case
        Nothing -> pure a
        Just sid -> do
          for_ attrs.controller \iid -> do
            pushAll [msg', skillTestModifier sid (toAbilitySource attrs 1) iid (DamageDealt 1)]
          pure . EnchantedBladeGuardian3 $ attrs `with` Metadata (Just sid)
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ | isJust (empowered meta) -> do
      for_ attrs.controller \iid -> do
        let drawing = drawCards iid (toAbilitySource attrs 1) 1
        pushAll [drawing, HealHorror (toTarget iid) (attrs.ability 1) 1]
      pure a
    RepeatSkillTest sid st | Just st.id == empowered meta -> do
      pure . EnchantedBladeGuardian3 $ attrs `with` Metadata (Just sid)
    SkillTestEnds sid _ _ | Just sid == empowered meta -> do
      pure . EnchantedBladeGuardian3 $ attrs `with` Metadata Nothing
    _ -> EnchantedBladeGuardian3 . (`with` meta) <$> runMessage msg attrs
