module Arkham.Treachery.Cards.SerpentsIre (serpentsIre) where

import Arkham.Enemy.Types (Enemy, Field (..))
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Spawn
import Arkham.Trait (Trait (Serpent))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted
import Arkham.Zone

newtype Metadata = Metadata {selectedEnemy :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SerpentsIre = SerpentsIre (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsIre :: TreacheryCard SerpentsIre
serpentsIre = treachery (SerpentsIre . (`with` Metadata Nothing)) Cards.serpentsIre

instance RunMessage SerpentsIre where
  runMessage msg t@(SerpentsIre (attrs `With` meta)) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      serpents <- select $ OutOfPlayEnemy PursuitZone $ EnemyWithTrait Serpent
      fightValue <-
        maybeFieldMax @(OutOfPlayEntity 'PursuitZone Enemy)
          (OutOfPlayEnemyField PursuitZone EnemyFight)
          (OutOfPlayEnemy PursuitZone $ EnemyWithTrait Serpent)
      choices <-
        filterM
          ( fieldMap @(OutOfPlayEntity 'PursuitZone Enemy)
              (OutOfPlayEnemyField PursuitZone EnemyFight)
              (== Just fightValue)
          )
          serpents
      if null choices
        then gainSurge attrs
        else withLocationOf iid \lid -> do
          chooseTargetM iid choices \eid -> do
            push
              $ EnemySpawn
              $ SpawnDetails
                { spawnDetailsInvestigator = Just iid
                , spawnDetailsSpawnAt = SpawnAtLocation lid
                , spawnDetailsEnemy = eid
                , spawnDetailsOverridden = False
                }
            handleTarget iid attrs eid
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      spawned <- selectAny $ EnemyWithId eid
      if spawned
        then do
          sid <- getRandom
          revelationSkillTest sid iid attrs #agility (EnemyMaybeFieldCalculation eid EnemyFight)
          pure . SerpentsIre $ attrs `with` Metadata (Just eid)
        else pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      case selectedEnemy meta of
        Nothing -> error "enemy must be set"
        Just eid -> initiateEnemyAttack eid attrs iid
      pure . SerpentsIre $ attrs `with` Metadata Nothing
    _ -> SerpentsIre . (`with` meta) <$> liftRunMessage msg attrs
