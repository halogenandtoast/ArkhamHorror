module Arkham.Story.Cards.UnfinishedBusiness_L (
  UnfinishedBusiness_L (..),
  unfinishedBusiness_L,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Matcher
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype UnfinishedBusiness_L = UnfinishedBusiness_L StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

unfinishedBusiness_L :: StoryCard UnfinishedBusiness_L
unfinishedBusiness_L = story UnfinishedBusiness_L Cards.unfinishedBusiness_L

instance RunMessage UnfinishedBusiness_L where
  runMessage msg s@(UnfinishedBusiness_L attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      alreadyResolved <- getAlreadyResolved attrs

      let card = lookupCard Enemies.heretic_K (toCardId attrs)
      -- enemy <- selectJust $ enemyIs Enemies.heretic_K
      if alreadyResolved
        then do
          send $ format card <> " was \"Banished\""
          push $ AddToVictory (toTarget attrs)
        else do
          creation <- createEnemy card (storyPlacement attrs)
          let enemy = enemyCreationEnemyId creation
          afterStoryResolution
            attrs
            [RemoveStory $ toId attrs, toMessage creation, InitiateEnemyAttack $ enemyAttack enemy attrs iid]
      pure s
    ResolveStory iid DoNotResolveIt story' | story' == toId attrs -> do
      player <- getPlayer iid
      push $ chooseOne player [AbilityLabel iid (mkAbility attrs 1 $ ForcedAbility AnyWindow) [] []]
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      enemy <- selectJust $ enemyIs Enemies.heretic_K
      afterStoryResolution attrs [InitiateEnemyAttack $ enemyAttack enemy attrs iid]
      pure s
    _ -> UnfinishedBusiness_L <$> runMessage msg attrs
