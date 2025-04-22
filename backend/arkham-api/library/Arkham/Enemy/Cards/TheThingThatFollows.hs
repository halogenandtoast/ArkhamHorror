module Arkham.Enemy.Cards.TheThingThatFollows (theThingThatFollows) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype TheThingThatFollows = TheThingThatFollows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingThatFollows :: EnemyCard TheThingThatFollows
theThingThatFollows =
  enemyWith
    TheThingThatFollows
    Cards.theThingThatFollows
    (3, Static 2, 3)
    (1, 1)
    $ (spawnAtL ?~ SpawnAt (FarthestLocationFromYou Anywhere))
    . (\a -> a & preyL .~ BearerOf (toId a))

instance HasAbilities TheThingThatFollows where
  getAbilities (TheThingThatFollows x) =
    extend1 x $ mkAbility x 1 $ forced $ EnemyWouldBeDefeated #when (be x)

instance RunMessage TheThingThatFollows where
  runMessage msg e@(TheThingThatFollows attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      eliminated <- selectNone $ InvestigatorWithId iid
      if eliminated
        then removeFromGame attrs
        else do
          nonEmptyDeck <- fieldMap InvestigatorDeck (not . null) iid
          when nonEmptyDeck do
            cancelEnemyDefeat attrs.id
            shuffleIntoDeck iid attrs
      pure e
    _ -> TheThingThatFollows <$> liftRunMessage msg attrs
