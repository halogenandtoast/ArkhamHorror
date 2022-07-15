{-# LANGUAGE TemplateHaskell #-}
module Arkham.UIMessage where

import Arkham.Prelude

import Arkham.ChaosBagStepState
import Arkham.Ability
import Arkham.Id
import Arkham.Card
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Window
import Data.Aeson.TH

data UIMessage msg
  = Label Text [msg]
  | TargetLabel Target [msg]
  | CardLabel CardCode [msg]
  | SkillLabel SkillType [msg]
  | EvadeLabel EnemyId [msg]
  | ComponentLabel Component [msg]
  | ChooseTokenGroups Source InvestigatorId ChaosBagStep
  | Unlabeled [msg]
  | UseAbility InvestigatorId Ability [Window]
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''UIMessage)

done :: Text -> UIMessage msg
done txt = Label txt []
