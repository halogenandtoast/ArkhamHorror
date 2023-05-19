module Arkham.Story.Cards.UnfinishedBusiness_J (
  UnfinishedBusiness_J (..),
  unfinishedBusiness_J,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_J = UnfinishedBusiness_J StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_J :: StoryCard UnfinishedBusiness_J
unfinishedBusiness_J = story UnfinishedBusiness_J Cards.unfinishedBusiness_J

instance RunMessage UnfinishedBusiness_J where
  runMessage msg s@(UnfinishedBusiness_J attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      alreadyResolved <- getAlreadyResolved attrs
      enemy <- selectJust $ enemyIs Enemies.heretic_I
      if alreadyResolved
        then do
          card <- field EnemyCard enemy
          send $ format card <> " was \"Banished\""
          pushAll [RemoveEnemy enemy, AddToVictory (toTarget attrs)]
        else do
          afterStoryResolution attrs $ InitiateEnemyAttack $ enemyAttack enemy iid
          afterStoryResolution attrs $ gameModifier attrs enemy CannotBeDefeated
      -- TODO: parley only at spectral, +1 per player clues costt
      push $ chooseOne iid [targetLabel (toTarget attrs) []]
      pure s
    ResolveStory iid DoNotResolveIt story' | story' == toId attrs -> do
      push $ chooseOne iid [AbilityLabel iid (mkAbility attrs 1 $ ForcedAbility AnyWindow) [] []]
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ ResolveStory iid ResolveIt (toId attrs)
      pure s
    _ -> UnfinishedBusiness_J <$> runMessage msg attrs
