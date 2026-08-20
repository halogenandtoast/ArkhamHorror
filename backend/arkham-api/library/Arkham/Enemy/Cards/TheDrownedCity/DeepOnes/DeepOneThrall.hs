module Arkham.Enemy.Cards.TheDrownedCity.DeepOnes.DeepOneThrall (deepOneThrall) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheDrownedCity.DeepOnes qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Matcher

newtype DeepOneThrall = DeepOneThrall EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deepOneThrall :: EnemyCard DeepOneThrall
deepOneThrall = enemy DeepOneThrall Cards.deepOneThrall

instance HasAbilities DeepOneThrall where
  getAbilities (DeepOneThrall a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyEngaged #after You (be a)

instance RunMessage DeepOneThrall where
  runMessage msg e@(DeepOneThrall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscard iid (attrs.ability 1)
      pure e
    _ -> DeepOneThrall <$> liftRunMessage msg attrs
