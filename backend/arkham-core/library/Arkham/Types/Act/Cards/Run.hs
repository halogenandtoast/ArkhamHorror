module Arkham.Types.Act.Cards.Run
  ( Run(..)
  , run
  ) where

import Arkham.Prelude

import qualified Arkham.Act.Cards as Cards
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Classes
import Arkham.Types.Message hiding (Run)
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target

newtype Run = Run ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasActions, HasModifiersFor env)

run :: ActCard Run
run = act (1, A) Run Cards.run Nothing

instance ActRunner env => RunMessage env Run where
  runMessage msg a@(Run attrs@ActAttrs {..}) = case msg of
    WhenEnterLocation iid lid -> do
      isEngineCar <- elem lid <$> getLocationIdWithTitle "Engine Car"
      if isEngineCar
        then do
          pushAll
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
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} SkillAgility _
      | isSource attrs source && actSequence == Act 1 B -> a
      <$ push (SufferTrauma iid 1 0)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} SkillCombat _
      | isSource attrs source && actSequence == Act 1 B -> a
      <$ push (SufferTrauma iid 1 0)
    _ -> Run <$> runMessage msg attrs
