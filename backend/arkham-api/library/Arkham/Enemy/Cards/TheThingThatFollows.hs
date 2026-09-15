module Arkham.Enemy.Cards.TheThingThatFollows (theThingThatFollows) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ThePathToCarcosa qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype TheThingThatFollows = TheThingThatFollows EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theThingThatFollows :: EnemyCard TheThingThatFollows
theThingThatFollows =
  enemyWith TheThingThatFollows Cards.theThingThatFollows
    $ (spawnAtL ?~ SpawnAt (FarthestLocationFromYou Anywhere))
    . preyIsOnlyBearer

instance HasAbilities TheThingThatFollows where
  getAbilities (TheThingThatFollows x) =
    extend1 x $ restricted x 1 criteria $ forced $ EnemyWouldBeDefeated #when (be x)
   where
    -- If there is nowhere to shuffle it back to, the ability doesn't trigger at
    -- all and it is defeated normally: an empty deck, or one that can't be
    -- manipulated (The Harbinger on top). An eliminated bearer still triggers so
    -- that it can be removed from the game.
    criteria = case enemyBearer x of
      Nothing -> Never
      Just iid ->
        oneOf [notExists (InvestigatorWithId iid), exists (InvestigatorWithId iid <> CanShuffleIn)]

instance RunMessage TheThingThatFollows where
  runMessage msg e@(TheThingThatFollows attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      for_ (enemyBearer attrs) \iid -> do
        eliminated <- selectNone $ InvestigatorWithId iid
        if eliminated
          then removeFromGame attrs
          else do
            cancelEnemyDefeat attrs.id
            shuffleIntoDeck iid attrs
      pure e
    _ -> TheThingThatFollows <$> liftRunMessage msg attrs
