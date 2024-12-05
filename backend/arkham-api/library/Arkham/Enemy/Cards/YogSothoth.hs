module Arkham.Enemy.Cards.YogSothoth (yogSothoth, yogSothothEffect, YogSothoth (..)) where

import Arkham.Ability
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose

newtype YogSothoth = YogSothoth EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothoth :: EnemyCard YogSothoth
yogSothoth = enemyWith YogSothoth Cards.yogSothoth (4, Static 4, 0) (1, 5) (evadeL .~ Nothing)

instance HasModifiersFor YogSothoth where
  getModifiersFor (YogSothoth a) = do
    healthModifier <- perPlayer 6
    modifySelf a [HealthModifier healthModifier, CannotMakeAttacksOfOpportunity, CannotBeEvaded]

instance HasAbilities YogSothoth where
  getAbilities (YogSothoth a) =
    extend1 a $ mkAbility a 1 $ freeReaction (EnemyAttacks #when You AnyEnemyAttack $ be a)

instance RunMessage YogSothoth where
  runMessage msg e@(YogSothoth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseOneM iid $ for_ [0 .. attrs.sanityDamage] \n -> do
        let label =
              "Discard the top "
                <> tshow n
                <> " cards and take "
                <> tshow (attrs.sanityDamage - n)
                <> " horror"
        labeled label do
          enemyAttackModifier (attrs.ability 1) attrs $ HorrorDealt (-n)
          eid <- createCardEffectCapture Cards.yogSothoth (effectInt n) (attrs.ability 1) iid
          push $ DiscardTopOfDeck iid n (attrs.ability 1) Nothing
          disable eid
          when (5 - n > 0) $ assignHorror iid (attrs.ability 1) (5 - n)
      pure e
    _ -> YogSothoth <$> liftRunMessage msg attrs

newtype YogSothothEffect = YogSothothEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yogSothothEffect :: EffectArgs -> YogSothothEffect
yogSothothEffect = cardEffect YogSothothEffect Cards.yogSothoth

instance RunMessage YogSothothEffect where
  runMessage msg e@(YogSothothEffect attrs) = runQueueT $ case msg of
    Msg.DeckHasNoCards iid _ | isTarget iid attrs.target -> do
      drivenInsane iid
      pure e
    _ -> YogSothothEffect <$> liftRunMessage msg attrs
