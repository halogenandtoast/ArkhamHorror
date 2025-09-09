module Arkham.Asset.Assets.ButterflySwords5 (butterflySwords5) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Action (hasFightActions)
import Arkham.Helpers.SkillTest (getSkillTestId, getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype ButterflySwords5 = ButterflySwords5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

butterflySwords5 :: AssetCard ButterflySwords5
butterflySwords5 = asset ButterflySwords5 Cards.butterflySwords5

instance HasAbilities ButterflySwords5 where
  getAbilities (ButterflySwords5 attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage ButterflySwords5 where
  runMessage msg a@(ButterflySwords5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid attrs iid $ SkillModifier #combat 2
      chooseFightEnemy sid iid (attrs.ability 1)
      pure $ ButterflySwords5 $ setMeta @Int 0 attrs
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestId >>= \case
        Nothing -> pure a
        Just sid -> do
          let n = toResult @Int attrs.meta
          when ((n + 1 == 2) && not attrs.exhausted) do
            msgs <- capture $ withCost iid (exhaust attrs) $ do
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
            chooseOne iid [Label "Do not exhaust" [], Label "Exhaust to do +1 damage" msgs]

          pure $ ButterflySwords5 $ setMeta @Int (n + 1) attrs
    AfterSkillTestEnds (isAbilitySource attrs 1 -> True) _ _ -> do
      iid <- fromJustNote "no investigator" <$> getSkillTestInvestigator
      oncePerAbility attrs 1 do
        sid <- getRandom
        canFight <- hasFightActions iid (attrs.ability 1) (DuringTurn You) (defaultWindows iid)
        fight <- capture do
          skillTestModifier sid attrs iid $ AddSkillValue #agility
          chooseFightEnemy sid iid (attrs.ability 1)
        chooseOrRunOne iid $ Label "Do not fight again" [] : [Label "Fight again" fight | canFight]
      pure a
    _ -> ButterflySwords5 <$> liftRunMessage msg attrs
