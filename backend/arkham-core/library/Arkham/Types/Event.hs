module Arkham.Types.Event
  ( allEvents
  )
where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import ClassyPrelude
import qualified Data.HashSet as HashSet

allEvents
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => CardCode
  -> InvestigatorId
  -> m ()
allEvents "01022" = evidence
allEvents "01023" = dodge
allEvents "01024" = dynamiteBlast
allEvents "01036" = mindOverMatter
allEvents "01037" = workingAHunch
allEvents "01088" = emergencyCache
allEvents evid = const (throwString $ "No event with id: " <> show evid)

evidence
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
evidence iid = do
  currentLocationId <- asks (getId @LocationId iid)
  clueCount <- unClueCount <$> asks (getCount currentLocationId)
  if clueCount > 0
    then unshiftMessage (DiscoverCluesAtLocation iid currentLocationId 1)
    else pure ()

dodge
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
dodge _ = unshiftMessage CancelNextAttack

dynamiteBlast
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
dynamiteBlast iid = do
  currentLocationId <- asks (getId @LocationId iid)
  connectedLocationIds <-
    HashSet.toList . HashSet.map unConnectedLocationId <$> asks
      (getSet currentLocationId)
  choices <- for (currentLocationId : connectedLocationIds) $ \lid -> do
    enemyIds <- HashSet.toList <$> asks (getSet lid)
    investigatorIds <- HashSet.toList <$> asks (getSet @InvestigatorId lid)
    pure
      $ map (\eid -> EnemyDamage eid iid (EventSource "01023") 3) enemyIds
      <> map
           (\iid' -> InvestigatorDamage iid' (EventSource "01023") 3 0)
           investigatorIds
  unshiftMessage (Ask $ ChooseOne $ concat choices)


mindOverMatter
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
mindOverMatter iid = unshiftMessages
  [ AddModifier
    (InvestigatorTarget iid)
    (UseSkillInPlaceOf SkillCombat SkillIntellect (EventSource "01036"))
  , AddModifier
    (InvestigatorTarget iid)
    (UseSkillInPlaceOf SkillAgility SkillIntellect (EventSource "01036"))
  ]

workingAHunch
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
workingAHunch iid = do
  currentLocationId <- asks (getId @LocationId iid)
  clueCount <- unClueCount <$> asks (getCount currentLocationId)
  if clueCount > 0
    then unshiftMessage (DiscoverCluesAtLocation iid currentLocationId 1)
    else pure ()

blindingLight
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
blindingLight iid = do
  unshiftMessages( ChooseEvadeEnemy iid SkillWillpower False)
  SkillTestEnded _ tokens | inUse -> do
      when
          (any
            (`elem` [ Token.Skull
                    , Token.Cultist
                    , Token.Tablet
                    , Token.ElderThing
                    , Token.AutoFail
                    ]
            )
            tokens
          )
        $ unshiftMessage
            (LoseAction (getInvestigator attrs) (EventSource eventId))

emergencyCache
  :: (MonadReader env m, GameRunner env, MonadIO m) => InvestigatorId -> m ()
emergencyCache iid = unshiftMessage (TakeResources iid 3 False)
