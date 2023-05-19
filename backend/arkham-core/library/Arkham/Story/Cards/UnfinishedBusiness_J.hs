module Arkham.Story.Cards.UnfinishedBusiness_J (
  UnfinishedBusiness_J (..),
  unfinishedBusiness_J,
  unfinishedBusiness_JEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Projection
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (Spectral))

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
          afterStoryResolution attrs $
            createCardEffect Cards.unfinishedBusiness_J Nothing attrs (toTarget enemy)
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

newtype UnfinishedBusiness_JEffect = UnfinishedBusiness_JEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_JEffect :: EffectArgs -> UnfinishedBusiness_JEffect
unfinishedBusiness_JEffect = cardEffect UnfinishedBusiness_JEffect Cards.unfinishedBusiness_J

instance HasModifiersFor UnfinishedBusiness_JEffect where
  getModifiersFor target (UnfinishedBusiness_JEffect a) | effectTarget a == target =
    do
      pure $
        toModifiers a [CannotBeDefeated]
  getModifiersFor (AbilityTarget _ ability) (UnfinishedBusiness_JEffect a) = do
    case abilitySource ability of
      EnemySource eid | EnemyTarget eid == effectTarget a && abilityIndex ability == 1 -> do
        pure $
          toModifiers
            a
            [ SetAbilityCost $ ClueCost (StaticWithPerPlayer 1 1)
            , SetAbilityCriteria
                (CriteriaOverride $ OnSameLocation <> LocationExists (YourLocation <> LocationWithTrait Spectral))
            ]
      _ -> pure []
  getModifiersFor _ _ = pure []

instance RunMessage UnfinishedBusiness_JEffect where
  runMessage msg e@(UnfinishedBusiness_JEffect attrs@EffectAttrs {..}) = case msg of
    EndUpkeep -> do
      push (DisableEffect effectId)
      pure e
    _ -> UnfinishedBusiness_JEffect <$> runMessage msg attrs
