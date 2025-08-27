module Arkham.Enemy.Cards.AlejandroVela (alejandroVela, alejandroVelaEffect) where

import Arkham.Ability
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest.Lifted (getSkillTest, parley)
import Arkham.Message.Lifted.Story
import Arkham.Story.Cards qualified as Story

newtype AlejandroVela = AlejandroVela EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVela :: EnemyCard AlejandroVela
alejandroVela = enemy AlejandroVela Cards.alejandroVela (6, PerPlayer 4, 3) (1, 2)

instance HasAbilities AlejandroVela where
  getAbilities (AlejandroVela a) =
    extend1 a $ skillTestAbility $ restricted a 1 OnSameLocation parleyAction_

instance RunMessage AlejandroVela where
  runMessage msg e@(AlejandroVela attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      createCardEffect Cards.alejandroVela Nothing (attrs.ability 1) sid
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 5)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    Flip iid _ (isTarget attrs -> True) -> do
      anotherWay <- genCard Story.anotherWay
      resolveStoryWithTarget iid anotherWay attrs
      pure e
    _ -> AlejandroVela <$> liftRunMessage msg attrs

newtype AlejandroVelaEffect = AlejandroVelaEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

alejandroVelaEffect :: EffectArgs -> AlejandroVelaEffect
alejandroVelaEffect = cardEffect AlejandroVelaEffect Cards.alejandroVela

instance HasModifiersFor AlejandroVelaEffect where
  getModifiersFor (AlejandroVelaEffect a) = void $ runMaybeT do
    st <- MaybeT getSkillTest
    guard $ isTarget st.id a.target
    let tokens = filter ((== Tablet) . (.face)) st.revealedChaosTokens
    lift $ modifyEach a (map ChaosTokenTarget tokens) [ChangeChaosTokenModifier AutoSuccessModifier]

instance RunMessage AlejandroVelaEffect where
  runMessage msg e@(AlejandroVelaEffect attrs) = runQueueT $ case msg of
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> disableReturn e
    _ -> AlejandroVelaEffect <$> liftRunMessage msg attrs
