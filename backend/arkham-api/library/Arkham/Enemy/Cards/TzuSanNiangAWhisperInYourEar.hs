module Arkham.Enemy.Cards.TzuSanNiangAWhisperInYourEar (tzuSanNiangAWhisperInYourEar) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype TzuSanNiangAWhisperInYourEar = TzuSanNiangAWhisperInYourEar EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tzuSanNiangAWhisperInYourEar :: EnemyCard TzuSanNiangAWhisperInYourEar
tzuSanNiangAWhisperInYourEar = enemy TzuSanNiangAWhisperInYourEar Cards.tzuSanNiangAWhisperInYourEar (4, Static 3, 4) (1, 2)

instance HasAbilities TzuSanNiangAWhisperInYourEar where
  getAbilities (TzuSanNiangAWhisperInYourEar a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ SkillTestResult
        #when
        You
        (oneOf [WhileAttackingAnEnemy (be a), WhileEvadingAnEnemy (be a)])
        (SuccessResult $ atLeast 2)

instance RunMessage TzuSanNiangAWhisperInYourEar where
  runMessage msg e@(TzuSanNiangAWhisperInYourEar attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skeys <- select $ ScarletKeyWithPlacement (AttachedToEnemy attrs.id)
      chooseOneAtATimeM iid $ targets skeys shift
      pure e
    _ -> TzuSanNiangAWhisperInYourEar <$> liftRunMessage msg attrs
