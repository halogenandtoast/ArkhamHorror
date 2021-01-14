module Arkham.Types.Act.Cards.RunExclaim
  ( RunExclaim(..)
  , runExclaim
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner

newtype RunExclaim = RunExclaim Attrs
  deriving newtype (Show, ToJSON, FromJSON)

runExclaim :: RunExclaim
runExclaim = RunExclaim $ baseAttrs "02165" "Run!" (Act 1 A) Nothing

instance ActionRunner env => HasActions env RunExclaim where
  getActions iid window (RunExclaim attrs) = getActions iid window attrs

instance ActRunner env => RunMessage env RunExclaim where
  runMessage msg a@(RunExclaim attrs@Attrs {..}) = case msg of
    WhenEnterLocation iid lid -> do
      engineCar <- getId (LocationWithTitle "Engine Car")
      if engineCar == Just lid
        then do
          unshiftMessage
            (chooseOne
              iid
              [ Label "Attempt to dodge the creature" []
              , Label "Attempt to endure the creature's extreme heat" []
              ]
            )
          pure $ RunExclaim $ attrs & sequenceL .~ Act 1 B
        else pure a
    AdvanceAct aid _ | aid == actId && onSide A attrs -> pure a
    FailedSkillTest iid _ source _ _
      | isSource attrs source && actSequence == Act 1 A -> a
      <$ unshiftMessage (SufferTrauma iid 1 0)
    _ -> RunExclaim <$> runMessage msg attrs
