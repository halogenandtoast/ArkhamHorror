module Arkham.Homebrew.CircusExMortis.Enemies.BrashLothario (brashLothario) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Intimacy), hasVice, investigatorWithVice)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype BrashLothario = BrashLothario EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brashLothario :: EnemyCard BrashLothario
brashLothario = enemy BrashLothario Cards.brashLothario & setPrey (investigatorWithVice Intimacy)

instance HasAbilities BrashLothario where
  getAbilities (BrashLothario a) =
    extend
      a
      [ restricted a 1 (exists $ InvestigatorAt (locationWithEnemy a)) $ forced $ RoundEnds #when
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage BrashLothario where
  runMessage msg e@(BrashLothario attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt (locationWithEnemy attrs)) \iid -> do
        additional <- hasVice iid Intimacy
        chooseAndDiscardCards iid (attrs.ability 1) (if additional then 2 else 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      whenM (hasVice iid Intimacy) do
        skillTestModifier sid (attrs.ability 2) sid (Difficulty 2)
        selectEach (notInvestigator iid) \other ->
          skillTestModifier sid (attrs.ability 2) other (CannotCommitCards AnyCard)
      chooseSkillM iid [#willpower, #intellect] \sType ->
        parley sid iid (attrs.ability 2) attrs sType (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> BrashLothario <$> liftRunMessage msg attrs
