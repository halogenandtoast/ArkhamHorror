module Arkham.Enemy.Cards.Xzharah (xzharah) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Story
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Stories

newtype Xzharah = Xzharah EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

xzharah :: EnemyCard Xzharah
xzharah = enemy Xzharah Cards.xzharah

instance HasModifiersFor Xzharah where
  getModifiersFor (Xzharah a) = do
    n <- perPlayer 5
    modifySelf a [HealthModifier n, CannotBeDefeated]

instance HasAbilities Xzharah where
  getAbilities (Xzharah a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage Xzharah where
  runMessage msg e@(Xzharah attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      n <- getStrengthOfTheAbyss
      withI18n $ chooseOneM iid do
        labeled' "chooseWillpower" $ beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed n)
        labeled' "chooseIntellect" $ beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed n)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      readStoryWithPlacement iid attrs Stories.usurpTheNight (enemyPlacement attrs)
      pure e
    _ -> Xzharah <$> liftRunMessage msg attrs
