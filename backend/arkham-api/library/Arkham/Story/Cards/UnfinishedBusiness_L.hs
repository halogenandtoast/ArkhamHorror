module Arkham.Story.Cards.UnfinishedBusiness_L (unfinishedBusiness_L) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

{- HLint ignore "Use camelCase" -}

newtype UnfinishedBusiness_L = UnfinishedBusiness_L StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_L :: StoryCard UnfinishedBusiness_L
unfinishedBusiness_L = story UnfinishedBusiness_L Cards.unfinishedBusiness_L

instance RunMessage UnfinishedBusiness_L where
  runMessage msg s@(UnfinishedBusiness_L attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == attrs.id -> do
      let card = lookupCard Enemies.heretic_K (toCardId attrs)

      alreadyResolved <- getAlreadyResolved attrs
      if alreadyResolved
        then do
          send $ format card <> " was \"Banished\""
          addToVictory attrs
        else afterStoryResolution attrs do
          removeStory attrs
          enemy <- createEnemy card attrs.placement
          initiateEnemyAttack enemy attrs iid
      pure s
    ResolveStory iid DoNotResolveIt story' | story' == attrs.id -> do
      chooseOneM iid $ abilityLabeled iid (mkAbility attrs 1 $ forced AnyWindow) nothing
      pure s
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemy <- selectJust $ enemyIs Enemies.heretic_K
      afterStoryResolution attrs $ initiateEnemyAttack enemy attrs iid
      pure s
    _ -> UnfinishedBusiness_L <$> liftRunMessage msg attrs
