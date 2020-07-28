module Arkham.Types.Skill
  ( allSkills
  )
where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.GameRunner
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillTestResult
import Arkham.Types.Source
import ClassyPrelude

allSkills
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => CardCode
  -> InvestigatorId
  -> SkillTestResult
  -> m ()
allSkills "01025" = viciousBlow
allSkills "01039" = deduction
allSkills "01089" = guts
allSkills "01091" = overpower
allSkills "01092" = manualDexterity
allSkills "01093" = unexpectedCourage
allSkills skid =
  const (const (throwString $ "No event with id: " <> show skid))

viciousBlow
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => InvestigatorId
  -> SkillTestResult
  -> m ()
viciousBlow _ = \case
  SucceededBy _ ->
    unshiftMessage (SkillTestAddModifier (DamageDealt 1 (SkillSource "01025")))
  _ -> pure ()

deduction
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => InvestigatorId
  -> SkillTestResult
  -> m ()
deduction _ = \case
  SucceededBy _ -> unshiftMessage
    (SkillTestAddModifier (DiscoveredClues 1 (SkillSource "01039")))
  _ -> pure ()

guts
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => InvestigatorId
  -> SkillTestResult
  -> m ()
guts iid = \case
  SucceededBy _ -> unshiftMessage (AddOnSuccess (DrawCards iid 1))
  _ -> pure ()

overpower
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => InvestigatorId
  -> SkillTestResult
  -> m ()
overpower iid = \case
  SucceededBy _ -> unshiftMessage (AddOnSuccess (DrawCards iid 1))
  _ -> pure ()

manualDexterity
  :: (MonadReader env m, GameRunner env, MonadIO m)
  => InvestigatorId
  -> SkillTestResult
  -> m ()
manualDexterity iid = \case
  SucceededBy _ -> unshiftMessage (AddOnSuccess (DrawCards iid 1))
  _ -> pure ()

unexpectedCourage :: (MonadIO m) => InvestigatorId -> SkillTestResult -> m ()
unexpectedCourage _ _ = pure ()
