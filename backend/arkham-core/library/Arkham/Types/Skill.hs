module Arkham.Types.Skill
  ( module Arkham.Types.Skill
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Cards
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId

createSkill :: IsCard a => a -> InvestigatorId -> Skill
createSkill a iid = lookupSkill (toCardCode a) iid (SkillId $ toCardId a)

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
  | DoubleOrNothing' DoubleOrNothing
  | TrueUnderstanding' TrueUnderstanding
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef Skill where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance ActionRunner env => HasActions env Skill
deriving anyclass instance SkillRunner env => RunMessage env Skill
deriving anyclass instance HasModifiersFor env Skill

instance Entity Skill where
  type EntityId Skill = SkillId
  type EntityAttrs Skill = SkillAttrs

instance NamedEntity Skill where
  toName = toName . toAttrs

instance TargetEntity Skill where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Skill where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Skill where
  toCardId = toCardId . toAttrs

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
  , ("02026", (DoubleOrNothing' .) . doubleOrNothing)
  , ("04153", (TrueUnderstanding' .) . trueUnderstanding)
  ]

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . toAttrs
