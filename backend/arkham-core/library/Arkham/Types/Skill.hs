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
  | Guts' Guts
  | Perception' Perception
  | Overpower' Overpower
  | ManualDexterity' ManualDexterity
  | UnexpectedCourage' UnexpectedCourage
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

skillAttrs :: Skill -> Attrs
skillAttrs = \case
  ViciousBlow' attrs -> coerce attrs
  Deduction' attrs -> coerce attrs
  Opportunist' attrs -> coerce attrs
  Fearless' attrs -> coerce attrs
  Guts' attrs -> coerce attrs
  Perception' attrs -> coerce attrs
  Overpower' attrs -> coerce attrs
  ManualDexterity' attrs -> coerce attrs
  UnexpectedCourage' attrs -> coerce attrs

instance HasActions env investigator Skill where
  getActions i window = \case
    ViciousBlow' x -> getActions i window x
    Deduction' x -> getActions i window x
    Opportunist' x -> getActions i window x
    Fearless' x -> getActions i window x
    Guts' x -> getActions i window x
    Perception' x -> getActions i window x
    Overpower' x -> getActions i window x
    ManualDexterity' x -> getActions i window x
    UnexpectedCourage' x -> getActions i window x

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . skillAttrs

instance (SkillRunner env) => RunMessage env Skill where
  runMessage msg = \case
    ViciousBlow' x -> ViciousBlow' <$> runMessage msg x
    Deduction' x -> Deduction' <$> runMessage msg x
    Opportunist' x -> Opportunist' <$> runMessage msg x
    Fearless' x -> Fearless' <$> runMessage msg x
    Guts' x -> Guts' <$> runMessage msg x
    Perception' x -> Perception' <$> runMessage msg x
    Overpower' x -> Overpower' <$> runMessage msg x
    ManualDexterity' x -> ManualDexterity' <$> runMessage msg x
    UnexpectedCourage' x -> UnexpectedCourage' <$> runMessage msg x
