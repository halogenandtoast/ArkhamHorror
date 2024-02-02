module Arkham.Story.Cards.UnfinishedBusiness_J (
  UnfinishedBusiness_J (..),
  unfinishedBusiness_J,
  unfinishedBusiness_JEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Attack
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Trait (Trait (Spectral))

newtype UnfinishedBusiness_J = UnfinishedBusiness_J StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

unfinishedBusiness_J :: StoryCard UnfinishedBusiness_J
unfinishedBusiness_J = story UnfinishedBusiness_J Cards.unfinishedBusiness_J

instance RunMessage UnfinishedBusiness_J where
  runMessage msg s@(UnfinishedBusiness_J attrs) = case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      alreadyResolved <- getAlreadyResolved attrs
      mEnemy <- selectOne $ enemyIs Enemies.heretic_I
      if alreadyResolved
        then do
          let card = lookupCard Enemies.heretic_I (toCardId attrs)
          send $ format card <> " was \"Banished\""
          pushAll
            $ [RemoveEnemy enemy | enemy <- maybeToList mEnemy]
            <> [ReplaceCard (toCardId attrs) (toCard attrs), AddToVictory (toTarget attrs)]
        else case mEnemy of
          Just enemy -> do
            afterStoryResolution
              attrs
              [ RemoveStory $ toId attrs
              , createCardEffect Cards.unfinishedBusiness_J Nothing attrs (toTarget enemy)
              , InitiateEnemyAttack $ enemyAttack enemy attrs iid
              ]
          Nothing -> do
            let card = lookupCard Enemies.heretic_I (toCardId attrs)
            creation <- createEnemy card (storyPlacement attrs)
            let enemy = enemyCreationEnemyId creation
            afterStoryResolution
              attrs
              [ RemoveStory $ toId attrs
              , toMessage creation
              , createCardEffect Cards.unfinishedBusiness_J Nothing attrs (toTarget enemy)
              , InitiateEnemyAttack $ enemyAttack enemy attrs iid
              ]

      pure s
    ResolveStory iid DoNotResolveIt story' | story' == toId attrs -> do
      player <- getPlayer iid
      push $ chooseOne player [AbilityLabel iid (mkAbility attrs 1 $ ForcedAbility AnyWindow) [] []]
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ ResolveStory iid ResolveIt (toId attrs)
      pure s
    _ -> UnfinishedBusiness_J <$> runMessage msg attrs

newtype UnfinishedBusiness_JEffect = UnfinishedBusiness_JEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

unfinishedBusiness_JEffect :: EffectArgs -> UnfinishedBusiness_JEffect
unfinishedBusiness_JEffect = cardEffect UnfinishedBusiness_JEffect Cards.unfinishedBusiness_J

instance HasModifiersFor UnfinishedBusiness_JEffect where
  getModifiersFor target (UnfinishedBusiness_JEffect a) | effectTarget a == target =
    do
      pure
        $ toModifiers a [CannotBeDefeated]
  getModifiersFor (AbilityTarget _ ability) (UnfinishedBusiness_JEffect a) = do
    case abilitySource ability of
      EnemySource eid | EnemyTarget eid == effectTarget a && abilityIndex ability == 1 -> do
        pure
          $ toModifiers
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
