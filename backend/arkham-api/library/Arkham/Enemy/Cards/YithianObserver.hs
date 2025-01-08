module Arkham.Enemy.Cards.YithianObserver (YithianObserver (..), yithianObserver) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Projection

newtype YithianObserver = YithianObserver EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yithianObserver :: EnemyCard YithianObserver
yithianObserver =
  enemyWith YithianObserver Cards.yithianObserver (4, Static 4, 3) (1, 1)
    $ preyL
    .~ Prey FewestCardsInHand

instance HasAbilities YithianObserver where
  getAbilities (YithianObserver a) =
    extend1 a
      $ forcedAbility a 1
      $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage YithianObserver where
  runMessage msg e@(YithianObserver attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      emptyHand <- fieldMap InvestigatorHand null iid
      if emptyHand
        then nextSkillTestModifiers attrs attrs [DamageDealt 1, HorrorDealt 1]
        else randomDiscard iid attrs
      pure e
    _ -> YithianObserver <$> liftRunMessage msg attrs
