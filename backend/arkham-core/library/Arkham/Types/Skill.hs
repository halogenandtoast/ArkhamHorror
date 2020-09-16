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
import Arkham.Types.Skill.Cards
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import ClassyPrelude
import Data.Coerce
import Generics.SOP hiding (Generic)
import qualified Generics.SOP as SOP
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
  deriving anyclass (ToJSON, FromJSON, SOP.Generic)

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
skillAttrs = getAttrs

getAttrs :: (All2 IsAttrs (Code a), SOP.Generic a) => a -> Attrs
getAttrs a = go (unSOP $ from a)
 where
  go :: (All2 IsAttrs xs) => NS (NP I) xs -> Attrs
  go (S next) = go next
  go (Z (I x :* _)) = coerce x
  go (Z Nil) = error "should not happen"

class (Coercible a Attrs) => IsAttrs a
instance (Coercible a Attrs) => IsAttrs a
