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
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.SkillType
import Arkham.Target
import Arkham.Trait ( Trait (Serpent) )
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

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
      inPursuit <- getInPursuitEnemies
      serpents <-
        selectList $ SetAsideMatcher $ EnemyWithTrait Serpent <> EnemyOneOf
          (map EnemyWithId $ toList inPursuit)
      fightValue <- getMax0 <$> selectAgg
        Max
        (SetAsideEnemyField EnemyFight)
        (SetAsideMatcher $ EnemyOneOf (map EnemyWithId serpents))
      choices <-
        toList
        . setFromList @(HashSet EnemyId)
        <$> filterM
              (fieldMap (SetAsideEnemyField EnemyFight) ((== fightValue)))
              serpents
      if null choices
        then push $ Surge iid source
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
          fight <- field EnemyFight eid
          push $ RevelationSkillTest iid (toSource attrs) SkillAgility fight
          pure . SerpentsIre $ attrs `with` Metadata (Just eid)
        else pure t
    FailedSkillTest iid _ (isSource attrs -> True) SkillTestInitiatorTarget{} _ _
      -> do
        case selectedEnemy meta of
          Nothing -> error "enemy must be set"
          Just eid -> push $ InitiateEnemyAttack iid eid RegularAttack
        pure . SerpentsIre $ attrs `with` Metadata Nothing
    _ -> SerpentsIre . (`with` meta) <$> runMessage msg attrs
