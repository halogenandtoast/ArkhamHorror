module Arkham.Asset.Assets.TheFacePracticed (theFacePracticed) where

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

newtype TheFacePracticed = TheFacePracticed AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFacePracticed :: AssetCard TheFacePracticed
theFacePracticed = asset TheFacePracticed Cards.theFacePracticed

instance HasAbilities TheFacePracticed where
  getAbilities (TheFacePracticed a) =
    [controlled a 1 (exists $ EnemyAt YourLocation <> EnemyWithTrait Casino) parleyAction_]

instance RunMessage TheFacePracticed where
  runMessage msg a@(TheFacePracticed attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Casino
      sid <- getRandom
      chooseTargetM iid enemies \enemy -> do
        chooseBeginSkillTestEdit sid iid (attrs.ability 1) enemy [#willpower, #intellect] (Fixed 2) \st -> st {skillTestAction = Just #parley}
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
    _ -> TheFacePracticed <$> liftRunMessage msg attrs
