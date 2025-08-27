module Arkham.Enemy.Cards.KamanThah (kamanThah) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.GameValue
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Story
import Arkham.Story.Cards qualified as Story

newtype KamanThah = KamanThah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kamanThah :: EnemyCard KamanThah
kamanThah = enemy KamanThah Cards.kamanThah (2, Static 3, 2) (1, 0)

instance HasAbilities KamanThah where
  getAbilities (KamanThah x) =
    extend
      x
      [ skillTestAbility $ mkAbility x 1 parleyAction_
      , mkAbility x 2 $ forced $ EnemyDefeated #after You ByAny (be x)
      ]

instance RunMessage KamanThah where
  runMessage msg e@(KamanThah attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      sid <- getRandom
      chooseOneM iid do
        for_ [#willpower, #intellect] \kind ->
          skillLabeled kind
            $ parley sid iid (attrs.ability 1) iid kind (Fixed $ 2 + n)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      investigators <- select (InvestigatorAt $ locationWithEnemy attrs)
      chooseOneAtATimeM iid do
        targets investigators $ initiateEnemyAttack attrs (attrs.ability 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOverBy iid (attrs.ability 2) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      theTrialOfKamanThah <- genCard Story.theTrialOfKamanThah
      removeEnemy attrs
      resolveStory iid theTrialOfKamanThah
      pure e
    _ -> KamanThah <$> liftRunMessage msg attrs
