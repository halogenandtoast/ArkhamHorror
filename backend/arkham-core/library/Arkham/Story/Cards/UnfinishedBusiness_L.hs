module Arkham.Story.Cards.UnfinishedBusiness_L (
  UnfinishedBusiness_L (..),
  unfinishedBusiness_L,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_L = UnfinishedBusiness_L StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_L :: StoryCard UnfinishedBusiness_L
unfinishedBusiness_L = story UnfinishedBusiness_L Cards.unfinishedBusiness_L

instance RunMessage UnfinishedBusiness_L where
  runMessage msg s@(UnfinishedBusiness_L attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      alreadyResolved <- getAlreadyResolved attrs
      enemy <- selectJust $ enemyIs Enemies.heretic_K
      if alreadyResolved
        then do
          card <- field EnemyCard enemy
          send $ format card <> " was \"Banished\""
          pushAll [RemoveEnemy enemy, AddToVictory (toTarget attrs)]
        else do
          afterStoryResolution attrs $ InitiateEnemyAttack $ enemyAttack enemy attrs iid
      push $ chooseOne iid [targetLabel (toTarget attrs) []]
      pure s
    ResolveStory iid DoNotResolveIt story' | story' == toId attrs -> do
      push $ chooseOne iid [AbilityLabel iid (mkAbility attrs 1 $ ForcedAbility AnyWindow) [] []]
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.heretic_K
      afterStoryResolution attrs $ InitiateEnemyAttack $ enemyAttack enemy attrs iid
      pure s
    _ -> UnfinishedBusiness_L <$> runMessage msg attrs
