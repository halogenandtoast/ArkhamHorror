{- HLINT ignore "Use camelCase" -}
module Arkham.Story.Cards.UnfinishedBusiness_J (
  unfinishedBusiness_J,
  unfinishedBusiness_JEffect,
) where

import Arkham.Ability
import Arkham.Card
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Helpers.Ref
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement
import Arkham.Source
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted
import Arkham.Target
import Arkham.Trait (Trait (Spectral))

newtype UnfinishedBusiness_J = UnfinishedBusiness_J StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_J :: StoryCard UnfinishedBusiness_J
unfinishedBusiness_J = story UnfinishedBusiness_J Cards.unfinishedBusiness_J

instance RunMessage UnfinishedBusiness_J where
  runMessage msg s@(UnfinishedBusiness_J attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      alreadyResolved <- getAlreadyResolved attrs
      mEnemy <- selectOne $ enemyIs Enemies.heretic_I
      if alreadyResolved
        then do
          let card = lookupCard Enemies.heretic_I (toCardId attrs)
          send $ format card <> " was \"Banished\""
          for_ mEnemy (push . RemoveEnemy)
          push $ ReplaceCard attrs.cardId (toCard attrs)
          addToVictory attrs
        else case mEnemy of
          Just enemy -> do
            cancelEnemyDefeat enemy
            healAllDamage attrs enemy
            place enemy attrs.placement
            createCardEffect Cards.unfinishedBusiness_J Nothing attrs (toTarget enemy)
            afterStoryResolution attrs do
              removeStory attrs
              initiateEnemyAttack enemy attrs iid
          Nothing -> do
            let card = lookupCard Enemies.heretic_I (toCardId attrs)
            enemy <- createEnemy card attrs.placement
            createCardEffect Cards.unfinishedBusiness_J Nothing attrs (toTarget enemy)
            afterStoryResolution attrs do
              removeStory attrs
              initiateEnemyAttack enemy attrs iid

      pure s
    ResolveStory iid DoNotResolveIt story' | story' == toId attrs -> do
      chooseOneM iid $ abilityLabeled iid (mkAbility attrs 1 $ forced AnyWindow) nothing
      pure s
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      resolveStory iid attrs
      pure s
    _ -> UnfinishedBusiness_J <$> liftRunMessage msg attrs

newtype UnfinishedBusiness_JEffect = UnfinishedBusiness_JEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unfinishedBusiness_JEffect :: EffectArgs -> UnfinishedBusiness_JEffect
unfinishedBusiness_JEffect = cardEffect UnfinishedBusiness_JEffect Cards.unfinishedBusiness_J

instance HasModifiersFor UnfinishedBusiness_JEffect where
  getModifiersFor (UnfinishedBusiness_JEffect a) = do
    modified_ a a.target [CannotBeDefeated]
    selectOne (AbilityIs (targetToSource a.target) 1) >>= traverse_ \ab -> do
      investigators <- getInvestigators
      modifyEach
        a
        [AbilityTarget iid ab.ref | iid <- investigators]
        [ SetAbilityCost $ ClueCost (StaticWithPerPlayer 1 1)
        , SetAbilityCriteria
            (CriteriaOverride $ OnSameLocation <> LocationExists (YourLocation <> LocationWithTrait Spectral))
        ]

instance RunMessage UnfinishedBusiness_JEffect where
  runMessage msg e@(UnfinishedBusiness_JEffect attrs) = runQueueT $ case msg of
    EndUpkeep -> disableReturn e
    _ -> UnfinishedBusiness_JEffect <$> liftRunMessage msg attrs
