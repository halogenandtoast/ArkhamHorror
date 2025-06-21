module Arkham.Enemy.Cards.JohnnyValoneHereToCollect (
  johnnyValoneHereToCollect,
  JohnnyValoneHereToCollect(..),
) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype JohnnyValoneHereToCollect = JohnnyValoneHereToCollect EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

johnnyValoneHereToCollect :: EnemyCard JohnnyValoneHereToCollect
johnnyValoneHereToCollect =
  enemy JohnnyValoneHereToCollect Cards.johnnyValoneHereToCollect (3, Static 2, 2) (0, 1)

instance HasModifiersFor JohnnyValoneHereToCollect where
  getModifiersFor (JohnnyValoneHereToCollect a) = modifySelf a [CannotBeDamaged]

instance HasAbilities JohnnyValoneHereToCollect where
  getAbilities (JohnnyValoneHereToCollect a) =
    extend1 a $ mkAbility a 1 $ forced $ TurnEnds #when (InvestigatorAt $ locationWithEnemy a)

instance RunMessage JohnnyValoneHereToCollect where
  runMessage msg e@(JohnnyValoneHereToCollect attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ LoseResources iid (attrs.ability 1) 2
      pure e
    _ -> JohnnyValoneHereToCollect <$> liftRunMessage msg attrs
