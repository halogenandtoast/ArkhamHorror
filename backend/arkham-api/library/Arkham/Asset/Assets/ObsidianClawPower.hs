module Arkham.Asset.Assets.ObsidianClawPower (obsidianClawPower) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Helpers
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (getSkillTestSource, getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Matcher

newtype ObsidianClawPower = ObsidianClawPower AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianClawPower :: AssetCard ObsidianClawPower
obsidianClawPower = asset ObsidianClawPower Cards.obsidianClawPower

instance HasModifiersFor ObsidianClawPower where
  getModifiersFor (ObsidianClawPower a) = do
    artifactModifiers a
    -- "If the attacked enemy is exhausted, this attack deals +1 damage." Read at
    -- modifier time so it follows the enemy actually targeted by the attack.
    for_ a.controller \iid -> maybeModified_ a iid do
      -- Gated on this card's own attack; the bonus is not a general one.
      (isAbilitySource a 1 -> True) <- MaybeT getSkillTestSource
      eid <- MaybeT getSkillTestTargetedEnemy
      guardM $ eid <=~> ExhaustedEnemy
      pure [DamageDealt 1]

instance HasAbilities ObsidianClawPower where
  getAbilities (ObsidianClawPower a) =
    [ controlled_ a 1 $ fightActionWithAlternate #agility (exhaust a)
    , cardI18n (withI18nTooltip "obsidianClaw.flip")
        $ limited (MaxPer Cards.obsidianClawPower PerRound 1)
        $ controlled_ a 2
        $ FastAbility Free
    , artifactAbility a 3
    ]

instance RunMessage ObsidianClawPower where
  runMessage msg a@(ObsidianClawPower attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- "You may use your [agility] instead of your [combat] and get +1 skill
      -- value for this attack." The +1 applies either way, so it rides on the
      -- test rather than on the chosen skill.
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (AnySkillValue 1)
      chooseFightEnemyWithSkillChoice sid iid (attrs.ability 1) [#agility, #combat]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      handOffArtifact iid attrs
      pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.obsidianClaw
      pure a
    _ -> ObsidianClawPower <$> liftRunMessage msg attrs
