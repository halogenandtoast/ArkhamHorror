module Arkham.Asset.Assets.TheFaceUnpracticed (theFaceUnpracticed) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Query (getInvestigators)
import Arkham.I18n
import Arkham.Matcher hiding (InvestigatorEliminated)
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.SkillTest.Base
import Arkham.Trait (Trait (Casino))

newtype TheFaceUnpracticed = TheFaceUnpracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFaceUnpracticed :: AssetCard TheFaceUnpracticed
theFaceUnpracticed = asset TheFaceUnpracticed Cards.theFaceUnpracticed

instance HasAbilities TheFaceUnpracticed where
  getAbilities (TheFaceUnpracticed a) =
    [controlled a 1 (exists $ EnemyAt YourLocation <> EnemyWithTrait Casino) parleyAction_]

instance RunMessage TheFaceUnpracticed where
  runMessage msg a@(TheFaceUnpracticed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Casino
      sid <- getRandom
      chooseTargetM iid enemies \enemy -> do
        chooseBeginSkillTestEdit sid iid (attrs.ability 1) enemy [#willpower, #intellect] (Fixed 3) \st -> st {skillTestAction = Just #parley}
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      investigators <- getInvestigators >>= filterM \iid' -> (> 0) <$> getAlarmLevel iid'
      when (attrs.ready && notNull investigators) do
        chooseOneM iid $ withI18n do
          targets investigators \iid' -> do
            exhaustThis attrs
            reduceAlarmLevel (attrs.ability 1) iid'
          skip_
      pure a
    InvestigatorEliminated _ -> pure a
    Flip _ _ (isTarget attrs -> True) -> do
      push $ ReplaceAsset attrs.id Cards.theFacePracticed
      pure a
    _ -> TheFaceUnpracticed <$> liftRunMessage msg attrs
