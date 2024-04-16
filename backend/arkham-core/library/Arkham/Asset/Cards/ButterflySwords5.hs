module Arkham.Asset.Cards.ButterflySwords5 (butterflySwords5, ButterflySwords5 (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Game.Helpers (hasFightActions)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
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
      skillTestModifier attrs iid $ SkillModifier #combat 2
      chooseFightEnemy iid (attrs.ability 1)
      pure $ ButterflySwords5 $ setMeta @Int 0 attrs
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let n = toResult @Int attrs.meta
      when ((n + 1 == 2) && not attrs.exhausted) do
        msgs <- evalQueueT $ withCost iid (exhaust attrs) $ do
          skillTestModifier (attrs.ability 1) iid (DamageDealt 1)
        chooseOne iid [Label "Do not exhaust" [], Label "Exhaust to do +1 damage" msgs]

      pure $ ButterflySwords5 $ setMeta @Int (n + 1) attrs
    AfterSkillTestEnds (isAbilitySource attrs 1 -> True) _ _ | not attrs.exhausted -> do
      iid <- fromJustNote "no investigator" <$> getSkillTestInvestigator
      oncePerAbility attrs 1 $ afterSkillTest $ forInvestigator iid msg
      pure a
    ForInvestigator iid (AfterSkillTestEnds (isAbilitySource attrs 1 -> True) _ _) | not attrs.exhausted -> do
      canFight <- hasFightActions iid (DuringTurn You) (defaultWindows iid)
      fight <- evalQueueT do
        skillTestModifier attrs iid $ AddSkillValue #agility
        chooseFightEnemy iid (attrs.ability 1)
      chooseOrRunOne iid $ Label "Do not fight again" [] : [Label "Fight again" fight | canFight]
      pure a
    _ -> ButterflySwords5 <$> lift (runMessage msg attrs)
