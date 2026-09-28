module Arkham.Homebrew.CircusExMortis.Enemies.SadisticSocialite (sadisticSocialite) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Violence), hasVice, investigatorWithVice)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SadisticSocialite = SadisticSocialite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sadisticSocialite :: EnemyCard SadisticSocialite
sadisticSocialite = enemy SadisticSocialite Cards.sadisticSocialite & setPrey (investigatorWithVice Violence)

instance HasAbilities SadisticSocialite where
  getAbilities (SadisticSocialite a) =
    extend
      a
      [ restricted a 1 (exists $ InvestigatorAt (locationWithEnemy a)) $ forced $ RoundEnds #when
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage SadisticSocialite where
  runMessage msg e@(SadisticSocialite attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt (locationWithEnemy attrs)) \iid -> do
        additional <- hasVice iid Violence
        -- one assignment: the additional damage is part of the same instance
        assignDamage iid (attrs.ability 1) (if additional then 2 else 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      whenM (hasVice iid Violence) do
        skillTestModifier sid (attrs.ability 2) sid (Difficulty 2)
        selectEach (notInvestigator iid) \other ->
          skillTestModifier sid (attrs.ability 2) other (CannotCommitCards AnyCard)
      chooseSkillM iid [#willpower, #intellect] \sType ->
        parley sid iid (attrs.ability 2) attrs sType (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> SadisticSocialite <$> liftRunMessage msg attrs
