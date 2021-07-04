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
  = Deduction' Deduction
  | DoubleOrNothing' DoubleOrNothing
  | Fearless' Fearless
  | Guts' Guts
  | ManualDexterity' ManualDexterity
  | Opportunist' Opportunist
  | Overpower' Overpower
  | Perception' Perception
  | SurvivalInstinct' SurvivalInstinct
  | TrueUnderstanding' TrueUnderstanding
  | UnexpectedCourage' UnexpectedCourage
  | ViciousBlow' ViciousBlow
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
allSkills = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  [ Deduction' <$> deduction
  , DoubleOrNothing' <$> doubleOrNothing
  , Fearless' <$> fearless
  , Guts' <$> guts
  , ManualDexterity' <$> manualDexterity
  , Opportunist' <$> opportunist
  , Overpower' <$> overpower
  , Perception' <$> perception
  , SurvivalInstinct' <$> survivalInstinct
  , TrueUnderstanding' <$> trueUnderstanding
  , UnexpectedCourage' <$> unexpectedCourage
  , ViciousBlow' <$> viciousBlow
  ]

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . toAttrs
