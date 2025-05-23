module Arkham.Asset.Assets.Blackjack2 (blackjack2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (
  getSkillTestInvestigator,
  getSkillTestSource,
  getSkillTestTargetedEnemy,
 )
import Arkham.Matcher

newtype Blackjack2 = Blackjack2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackjack2 :: AssetCard Blackjack2
blackjack2 = asset Blackjack2 Cards.blackjack2

instance HasModifiersFor Blackjack2 where
  getModifiersFor (Blackjack2 attrs) = do
    getSkillTestInvestigator >>= \case
      Just iid -> maybeModified_ attrs iid do
        (isAbilitySource attrs 1 -> True) <- MaybeT getSkillTestSource
        eid <- MaybeT getSkillTestTargetedEnemy
        guardM $ lift $ selectAny $ investigatorEngagedWith eid <> notInvestigator iid
        pure [DamageDealt 1]
      Nothing -> pure mempty

instance HasAbilities Blackjack2 where
  getAbilities (Blackjack2 a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage Blackjack2 where
  runMessage msg a@(Blackjack2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DoesNotDamageOtherInvestigator]
      chooseFightEnemy sid iid source
      pure a
    _ -> Blackjack2 <$> liftRunMessage msg attrs
