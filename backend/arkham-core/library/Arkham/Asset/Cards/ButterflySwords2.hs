module Arkham.Asset.Cards.ButterflySwords2 (butterflySwords2, ButterflySwords2 (..)) where

import Arkham.Ability hiding (DuringTurn)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Classes.HasQueue (evalQueueT)
import Arkham.Game.Helpers (hasFightActions)
import Arkham.Helpers.SkillTest (getSkillTestInvestigator)
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Window (defaultWindows)

newtype ButterflySwords2 = ButterflySwords2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

butterflySwords2 :: AssetCard ButterflySwords2
butterflySwords2 = asset ButterflySwords2 Cards.butterflySwords2

instance HasAbilities ButterflySwords2 where
  getAbilities (ButterflySwords2 attrs) = [restrictedAbility attrs 1 ControlsThis fightAction_]

instance RunMessage ButterflySwords2 where
  runMessage msg a@(ButterflySwords2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier attrs iid $ SkillModifier #combat 1
      chooseFightEnemy iid (attrs.ability 1)
      pure a
    AfterSkillTestEnds (isAbilitySource attrs 1 -> True) _ _ | not attrs.exhausted -> do
      iid <- fromJustNote "no investigator" <$> getSkillTestInvestigator
      oncePerAbility attrs 1 $ afterSkillTest $ forInvestigator iid msg
      pure a
    ForInvestigator iid (AfterSkillTestEnds (isAbilitySource attrs 1 -> True) _ _) | not attrs.exhausted -> do
      canFight <- hasFightActions iid (DuringTurn You) (defaultWindows iid)
      fight <- evalQueueT $ withCost iid (exhaust attrs) do
        skillTestModifiers attrs iid [AddSkillValue #agility, DamageDealt 1]
        chooseFightEnemy iid (attrs.ability 1)
      chooseOrRunOne iid $ Label "Do not fight again" [] : [Label "Fight again" fight | canFight]
      pure a
    _ -> ButterflySwords2 <$> lift (runMessage msg attrs)
