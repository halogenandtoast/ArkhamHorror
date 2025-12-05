module Arkham.Enemy.Cards.TzuSanNiangTheLadyWithTheRedParasol (tzuSanNiangTheLadyWithTheRedParasol) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Placement
import Arkham.Trait (Trait (Geist))

newtype TzuSanNiangTheLadyWithTheRedParasol = TzuSanNiangTheLadyWithTheRedParasol EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tzuSanNiangTheLadyWithTheRedParasol :: EnemyCard TzuSanNiangTheLadyWithTheRedParasol
tzuSanNiangTheLadyWithTheRedParasol =
  enemy
    TzuSanNiangTheLadyWithTheRedParasol
    Cards.tzuSanNiangTheLadyWithTheRedParasol
    (2, PerPlayer 2, 2)
    (1, 1)

instance HasAbilities TzuSanNiangTheLadyWithTheRedParasol where
  getAbilities (TzuSanNiangTheLadyWithTheRedParasol a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyWouldBeDefeated #when (be a)

instance RunMessage TzuSanNiangTheLadyWithTheRedParasol where
  runMessage msg e@(TzuSanNiangTheLadyWithTheRedParasol attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      healAllDamage (attrs.ability 1) attrs
      push $ RemoveAllDoomFromPlay defaultRemoveDoomMatchers
      place attrs InTheShadows
      locations <- select $ LocationWithEnemy $ EnemyWithTrait Geist <> NonWeaknessEnemy
      gatherConcealedCards attrs.id >>= \case
        Just (TzuSanNiang, cards) -> do
          for_ cards $ push . CreateConcealedCard
          distributeEvenlyBetween cards locations
        _ -> error "failed to gather tzu san niang's concealed cards"
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      outForBlood <- genCard Cards.tzuSanNiangOutForBlood
      push $ ReplaceEnemy attrs.id outForBlood Swap
      pure e
    _ -> TzuSanNiangTheLadyWithTheRedParasol <$> liftRunMessage msg attrs
