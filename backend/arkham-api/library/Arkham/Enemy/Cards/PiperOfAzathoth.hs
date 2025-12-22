module Arkham.Enemy.Cards.PiperOfAzathoth (piperOfAzathoth) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype PiperOfAzathoth = PiperOfAzathoth EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, AsId)

piperOfAzathoth :: EnemyCard PiperOfAzathoth
piperOfAzathoth =
  enemy PiperOfAzathoth Cards.piperOfAzathoth (5, Static 7, 2) (0, 2)
    & setPrey LowestRemainingSanity

instance HasAbilities PiperOfAzathoth where
  getAbilities (PiperOfAzathoth a) = extend1 a $ mkAbility a 1 $ forced $ PhaseEnds #when #enemy

instance RunMessage PiperOfAzathoth where
  runMessage msg e@(PiperOfAzathoth attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      investigators <- select $ at_ (locationWithEnemy e) <> not_ (investigatorEngagedWith e)
      for_ investigators (initiateEnemyAttack attrs (attrs.ability 1))
      pure e
    _ -> PiperOfAzathoth <$> liftRunMessage msg attrs
