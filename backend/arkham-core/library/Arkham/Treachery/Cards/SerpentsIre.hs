module Arkham.Treachery.Cards.SerpentsIre
  ( serpentsIre
  , SerpentsIre(..)
  ) where

import Arkham.Prelude

import Arkham.Attack
import Arkham.Classes
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait ( Trait (Serpent) )
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner
import Arkham.Zone

newtype Metadata = Metadata { selectedEnemy :: Maybe EnemyId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SerpentsIre = SerpentsIre (TreacheryAttrs `With` Metadata)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serpentsIre :: TreacheryCard SerpentsIre
serpentsIre =
  treachery (SerpentsIre . (`with` Metadata Nothing)) Cards.serpentsIre

instance RunMessage SerpentsIre where
  runMessage msg t@(SerpentsIre (attrs `With` meta)) = case msg of
    Revelation iid source | isSource attrs source -> do
      serpents <-
        selectList $ OutOfPlayEnemy PursuitZone $ EnemyWithTrait Serpent
      fightValue <- selectMax
        (OutOfPlayEnemyField PursuitZone EnemyFight)
        (OutOfPlayEnemy PursuitZone $ EnemyWithTrait Serpent)
      choices <-
        toList
        . setFromList @(HashSet EnemyId)
        <$> filterM
              (fieldMap (OutOfPlayEnemyField PursuitZone EnemyFight) ((== fightValue)))
              serpents
      if null choices
        then push $ gainSurge attrs
        else do
          mlid <- field InvestigatorLocation iid
          for_ mlid $ \lid -> push $ chooseOne
            iid
            [ targetLabel
                eid
                [ EnemySpawn (Just iid) lid eid
                , HandleTargetChoice iid source (EnemyTarget eid)
                ]
            | eid <- choices
            ]
      pure t
    HandleTargetChoice iid (isSource attrs -> True) (EnemyTarget eid) -> do
      spawned <- selectAny $ EnemyWithId eid
      if spawned
        then do
          fightValue <- field EnemyFight eid
          push $ RevelationSkillTest iid (toSource attrs) SkillAgility fightValue
          pure . SerpentsIre $ attrs `with` Metadata (Just eid)
        else pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        case selectedEnemy meta of
          Nothing -> error "enemy must be set"
          Just eid -> push $ InitiateEnemyAttack $ enemyAttack eid iid
        pure . SerpentsIre $ attrs `with` Metadata Nothing
    _ -> SerpentsIre . (`with` meta) <$> runMessage msg attrs
