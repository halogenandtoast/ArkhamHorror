{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill
  ( lookupSkill
  , Skill(..)
  , ownerOfSkill
  )
where

import Arkham.Json
import Arkham.Types.Card (CardCode, HasCardCode(..))
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Cards
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import ClassyPrelude
import Data.Coerce
import Safe (fromJustNote)

data Skill
  = ViciousBlow' ViciousBlow
  | Deduction' Deduction
  | Opportunist' Opportunist
  | Fearless' Fearless
  | SurvivalInstinct' SurvivalInstinct
  | Guts' Guts
  | Perception' Perception
  | Overpower' Overpower
  | ManualDexterity' ManualDexterity
  | UnexpectedCourage' UnexpectedCourage
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasActions env investigator Skill
deriving anyclass instance (SkillRunner env) => RunMessage env Skill

instance HasCardCode Skill where
  getCardCode = skillCardCode . skillAttrs

lookupSkill :: CardCode -> (InvestigatorId -> SkillId -> Skill)
lookupSkill cardCode =
  fromJustNote ("Unknown skill: " <> show cardCode) $ lookup cardCode allSkills

allSkills :: HashMap CardCode (InvestigatorId -> SkillId -> Skill)
allSkills = mapFromList
  [ ("01025", (ViciousBlow' .) . viciousBlow)
  , ("01039", (Deduction' .) . deduction)
  , ("01053", (Opportunist' .) . opportunist)
  , ("01067", (Fearless' .) . fearless)
  , ("01081", (SurvivalInstinct' .) . survivalInstinct)
  , ("01089", (Guts' .) . guts)
  , ("01090", (Perception' .) . perception)
  , ("01091", (Overpower' .) . overpower)
  , ("01092", (ManualDexterity' .) . manualDexterity)
  , ("01093", (UnexpectedCourage' .) . unexpectedCourage)
  ]

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . skillAttrs

skillAttrs :: Skill -> Attrs
skillAttrs = \case
  ViciousBlow' attrs -> coerce attrs
  Deduction' attrs -> coerce attrs
  Opportunist' attrs -> coerce attrs
  Fearless' attrs -> coerce attrs
  SurvivalInstinct' attrs -> coerce attrs
  Guts' attrs -> coerce attrs
  Perception' attrs -> coerce attrs
  Overpower' attrs -> coerce attrs
  ManualDexterity' attrs -> coerce attrs
  UnexpectedCourage' attrs -> coerce attrs
