module Arkham.Enemy.Cards.TzuSanNiangOutForBlood (tzuSanNiangOutForBlood) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window (getPassedBy)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenarios.ShadesOfSuffering.Helpers
import Arkham.Token

newtype TzuSanNiangOutForBlood = TzuSanNiangOutForBlood EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tzuSanNiangOutForBlood :: EnemyCard TzuSanNiangOutForBlood
tzuSanNiangOutForBlood = enemy TzuSanNiangOutForBlood Cards.tzuSanNiangOutForBlood (2, Static 4, 2) (1, 1)

instance HasAbilities TzuSanNiangOutForBlood where
  getAbilities (TzuSanNiangOutForBlood a) =
    extend1 a
      $ restricted
        a
        1
        (exists $ scarletKeyIs Keys.theShadeReaper <> ScarletKeyWithTokens (atLeast 1) #charge)
      $ forced
      $ SkillTestResult
        #when
        You
        (oneOf [WhileEvadingAnEnemy (be a), WhileAttackingAnEnemy (be a)])
        (SuccessResult $ atLeast 1)

instance RunMessage TzuSanNiangOutForBlood where
  runMessage msg e@(TzuSanNiangOutForBlood attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getPassedBy -> n) _ -> do
      theShadeReaper <- selectJust (scarletKeyIs Keys.theShadeReaper)
      charges <- fieldMap ScarletKeyTokens (countTokens #charge) theShadeReaper
      let x = min n charges
      let half = x `div` 2
      removeTokens (attrs.ability 1) theShadeReaper #charge x
      if
        | x == 1 -> chooseOneM iid $ withI18n $ countVar 1 do
            labeled' "takeDamage" $ assignDamage iid (attrs.ability 1) 1
            labeled' "takeHorror" $ assignHorror iid (attrs.ability 1) 1
        | even x -> assignDamageAndHorror iid (attrs.ability 1) half half
        | otherwise -> do
            chooseOneM iid $ scenarioI18n do
              labeled' "tzuSanNiangOutForBlood.damage"
                $ assignDamageAndHorror iid (attrs.ability 1) (half + 1) half
              labeled' "tzuSanNiangOutForBlood.horror"
                $ assignDamageAndHorror iid (attrs.ability 1) half (half + 1)
      pure e
    _ -> TzuSanNiangOutForBlood <$> liftRunMessage msg attrs
