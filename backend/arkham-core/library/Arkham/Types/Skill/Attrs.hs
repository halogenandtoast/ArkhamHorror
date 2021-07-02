{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Skill.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.PlayerCard
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.SkillId
import Arkham.Types.Source
import Arkham.Types.Target

data SkillAttrs = SkillAttrs
  { skillCardDef :: CardDef
  , skillId :: SkillId
  , skillOwner :: InvestigatorId
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''SkillAttrs

instance HasCardDef SkillAttrs where
  toCardDef = skillCardDef

instance IsCard SkillAttrs where
  toCardId = unSkillId . skillId

instance ToJSON SkillAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "skill"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skill"

instance FromJSON SkillAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "skill"

instance Entity SkillAttrs where
  type EntityId SkillAttrs = SkillId
  type EntityAttrs SkillAttrs = SkillAttrs
  toId = skillId
  toAttrs = id

instance NamedEntity SkillAttrs where
  toName = cdName . toCardDef

instance TargetEntity SkillAttrs where
  toTarget = SkillTarget . skillId
  isTarget SkillAttrs { skillId } (SkillTarget sid) = skillId == sid
  isTarget _ _ = False

instance SourceEntity SkillAttrs where
  toSource = SkillSource . skillId
  isSource SkillAttrs { skillId } (SkillSource sid) = skillId == sid
  isSource _ _ = False

baseAttrs :: InvestigatorId -> SkillId -> CardCode -> SkillAttrs
baseAttrs iid eid cardCode = SkillAttrs
  { skillCardDef = lookupPlayerCardDef cardCode
  , skillId = eid
  , skillOwner = iid
  }

instance HasActions env SkillAttrs where
  getActions _ _ _ = pure []

instance RunMessage env SkillAttrs where
  runMessage _ a = pure a
