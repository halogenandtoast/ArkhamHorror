module Arkham.Types.Skill
  ( module Arkham.Types.Skill
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Name
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Cards
import Arkham.Types.Skill.Runner

createSkill :: IsCard a => a -> InvestigatorId -> Skill
createSkill a iid = lookupSkill (toCardCode a) iid (SkillId $ toCardId a)

data Skill
  = Deduction' Deduction
  | Deduction2' Deduction2
  | Defiance' Defiance
  | DoubleOrNothing' DoubleOrNothing
  | Fearless' Fearless
  | Fearless2' Fearless2
  | Guts' Guts
  | InquiringMind' InquiringMind
  | Leadership' Leadership
  | ManualDexterity' ManualDexterity
  | Opportunist' Opportunist
  | Opportunist2' Opportunist2
  | Overpower' Overpower
  | Perception' Perception
  | QuickThinking' QuickThinking
  | RiseToTheOccasion' RiseToTheOccasion
  | StrokeOfLuck2' StrokeOfLuck2
  | SurvivalInstinct' SurvivalInstinct
  | SurvivalInstinct2' SurvivalInstinct2
  | TrueUnderstanding' TrueUnderstanding
  | UnexpectedCourage' UnexpectedCourage
  | ViciousBlow' ViciousBlow
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance HasCardDef Skill where
  toCardDef = toCardDef . toAttrs

deriving anyclass instance ActionRunner env => HasActions env Skill
deriving anyclass instance SkillRunner env => RunMessage env Skill

instance HasModifiersFor env Skill where
  getModifiersFor = genericGetModifiersFor

instance Entity Skill where
  type EntityId Skill = SkillId
  type EntityAttrs Skill = SkillAttrs

instance Named Skill where
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
  , Deduction2' <$> deduction2
  , Defiance' <$> defiance
  , DoubleOrNothing' <$> doubleOrNothing
  , Fearless' <$> fearless
  , Fearless2' <$> fearless2
  , Guts' <$> guts
  , InquiringMind' <$> inquiringMind
  , Leadership' <$> leadership
  , ManualDexterity' <$> manualDexterity
  , Opportunist' <$> opportunist
  , Opportunist2' <$> opportunist2
  , Overpower' <$> overpower
  , Perception' <$> perception
  , QuickThinking' <$> quickThinking
  , RiseToTheOccasion' <$> riseToTheOccasion
  , StrokeOfLuck2' <$> strokeOfLuck2
  , SurvivalInstinct' <$> survivalInstinct
  , SurvivalInstinct2' <$> survivalInstinct2
  , TrueUnderstanding' <$> trueUnderstanding
  , UnexpectedCourage' <$> unexpectedCourage
  , ViciousBlow' <$> viciousBlow
  ]

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . toAttrs
