{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Skill
  ( module Arkham.Types.Skill
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Id
import Arkham.Types.Name
import Arkham.Types.Query
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Cards
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillTest

$(buildEntity "Skill")

createSkill :: IsCard a => a -> InvestigatorId -> Skill
createSkill a iid = lookupSkill (toCardCode a) iid (SkillId $ toCardId a)

instance HasCardCode Skill where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Skill where
  toCardDef = toCardDef . toAttrs

instance HasActions Skill where
  getActions = genericGetActions

instance
  ( HasSkillTest env
  , HasCount DamageCount env InvestigatorId
  , SkillRunner env
  )
  => RunMessage env Skill where
  runMessage = genericRunMessage

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

instance HasId OwnerId env Skill where
  getId = pure . OwnerId . ownerOfSkill

lookupSkill :: CardCode -> (InvestigatorId -> SkillId -> Skill)
lookupSkill cardCode =
  fromJustNote ("Unknown skill: " <> show cardCode) $ lookup cardCode allSkills

allSkills :: HashMap CardCode (InvestigatorId -> SkillId -> Skill)
allSkills = mapFromList $ map
  (cbCardCode &&& (curry . cbCardBuilder))
  $(buildEntityLookupList "Skill")

ownerOfSkill :: Skill -> InvestigatorId
ownerOfSkill = skillOwner . toAttrs
