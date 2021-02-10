{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Skill.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Name
import Arkham.Types.SkillId
import Arkham.Types.Source
import Arkham.Types.Target


import Arkham.Types.Skill.Runner
import Arkham.Types.Trait
import qualified Data.HashMap.Strict as HashMap

data SkillAttrs = SkillAttrs
  { skillName :: Text
  , skillId :: SkillId
  , skillCardCode :: CardCode
  , skillTraits :: HashSet Trait
  , skillOwner :: InvestigatorId
  , skillWeakness :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''SkillAttrs

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
  toName = mkName . skillName

instance TargetEntity SkillAttrs where
  toTarget = SkillTarget . skillId
  isTarget SkillAttrs { skillId } (SkillTarget sid) = skillId == sid
  isTarget _ _ = False

instance SourceEntity SkillAttrs where
  toSource = SkillSource . skillId
  isSource SkillAttrs { skillId } (SkillSource sid) = skillId == sid
  isSource _ _ = False

instance IsCard SkillAttrs where
  getCardId = unSkillId . skillId
  getCardCode = skillCardCode
  getTraits = skillTraits
  getKeywords = mempty

baseAttrs :: InvestigatorId -> SkillId -> CardCode -> SkillAttrs
baseAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player card: " <> unpack (unCardCode cardCode))
          (HashMap.lookup cardCode allPlayerCards)
        $ unSkillId eid
  in
    SkillAttrs
      { skillName = pcName
      , skillId = eid
      , skillCardCode = pcCardCode
      , skillTraits = pcTraits
      , skillOwner = iid
      , skillWeakness = False
      }

weaknessAttrs :: InvestigatorId -> SkillId -> CardCode -> SkillAttrs
weaknessAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          "missing weakness card"
          (HashMap.lookup cardCode allPlayerCards)
        $ unSkillId eid
  in
    SkillAttrs
      { skillName = pcName
      , skillId = eid
      , skillCardCode = pcCardCode
      , skillTraits = pcTraits
      , skillOwner = iid
      , skillWeakness = True
      }

instance HasActions env SkillAttrs where
  getActions _ _ _ = pure []

instance (SkillRunner env) => RunMessage env SkillAttrs where
  runMessage _ a = pure a
