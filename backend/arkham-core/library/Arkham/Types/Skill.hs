{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill
  ( lookupSkill
  , Skill(..)
  , ownerOfSkill
  )
where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Cards.Deduction
import Arkham.Types.Skill.Cards.Fearless
import Arkham.Types.Skill.Cards.Guts
import Arkham.Types.Skill.Cards.ManualDexterity
import Arkham.Types.Skill.Cards.Opportunist
import Arkham.Types.Skill.Cards.Overpower
import Arkham.Types.Skill.Cards.Perception
import Arkham.Types.Skill.Cards.SurvivalInstinct
import Arkham.Types.Skill.Cards.UnexpectedCourage
import Arkham.Types.Skill.Cards.ViciousBlow
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import ClassyPrelude
import Data.Coerce
import qualified Data.HashMap.Strict as HashMap
import Safe (fromJustNote)

lookupSkill :: CardCode -> (InvestigatorId -> SkillId -> Skill)
lookupSkill cardCode =
  fromJustNote ("Unknown skill: " <> show cardCode)
    $ HashMap.lookup cardCode allSkills

allSkills :: HashMap CardCode (InvestigatorId -> SkillId -> Skill)
allSkills = HashMap.fromList
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

instance HasCardCode Skill where
  getCardCode = skillCardCode . skillAttrs

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

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . skillAttrs
