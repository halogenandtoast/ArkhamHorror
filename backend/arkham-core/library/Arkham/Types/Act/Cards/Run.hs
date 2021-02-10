module Arkham.Types.Act.Cards.Run
  ( Run(..)
  , run
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.Message hiding (Run)
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner

newtype Run = Run ActAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

run :: Run
run = Run $ baseAttrs "02165" "Run!" (Act 1 A) Nothing

instance ActionRunner env => HasActions env Run where
  getActions iid window (Run attrs) = getActions iid window attrs

instance ActRunner env => RunMessage env Run where
  runMessage msg a@(Run attrs@ActAttrs {..}) = case msg of
    WhenEnterLocation iid lid -> do
      isEngineCar <- elem lid <$> getLocationIdWithTitle "Engine Car"
      if isEngineCar
        then do
          unshiftMessages
            (chooseOne
                iid
                [ Label
                  "Attempt to dodge the creature"
                  [ BeginSkillTest
                      iid
                      (ActSource actId)
                      (ActTarget actId)
                      Nothing
                      SkillAgility
                      3
                  ]
                , Label
                  "Attempt to endure the creature's extreme heat"
                  [ BeginSkillTest
                      iid
                      (ActSource actId)
                      (ActTarget actId)
                      Nothing
                      SkillCombat
                      3
                  ]
                ]
            : [NextAct actId "02166"]
            )
          pure $ Run $ attrs & sequenceL .~ Act 1 B
        else pure a
    FailedSkillTest iid _ source _ SkillAgility _
      | isSource attrs source && actSequence == Act 1 B -> a
      <$ unshiftMessage (SufferTrauma iid 1 0)
    FailedSkillTest iid _ source _ SkillCombat _
      | isSource attrs source && actSequence == Act 1 B -> a
      <$ unshiftMessage (SufferTrauma iid 1 0)
    _ -> Run <$> runMessage msg attrs
