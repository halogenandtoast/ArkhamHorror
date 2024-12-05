module Arkham.Asset.Assets.Machete (Machete (..), machete) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Matcher
import Arkham.Prelude

newtype Machete = Machete AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

machete :: AssetCard Machete
machete = asset Machete Cards.machete

instance HasModifiersFor Machete where
  getModifiersFor (Machete a) = case a.controller of
    Nothing -> pure mempty
    Just iid -> maybeModified_ a iid do
      guardM $ isAbilitySource a 1 <$> MaybeT getSkillTestSource
      EnemyTarget eid <- MaybeT getSkillTestTarget
      guardM $ (== [eid]) <$> lift (select $ enemyEngagedWith iid)
      guardM $ (== iid) <$> MaybeT getSkillTestInvestigator
      pure [DamageDealt 1]

instance HasAbilities Machete where
  getAbilities (Machete a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage Machete where
  runMessage msg a@(Machete attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifier sid source iid (SkillModifier #combat 1)
      pushAll [enabled, chooseFight]
      pure a
    _ -> Machete <$> runMessage msg attrs
