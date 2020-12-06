{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Attrs where

import Arkham.Import

import Arkham.Types.Skill.Runner
import Arkham.Types.Trait
import qualified Data.HashMap.Strict as HashMap

instance HasAttrs Attrs where
  type AttrsT Attrs = Attrs
  toAttrs = id

data Attrs = Attrs
  { skillName :: Text
  , skillId :: SkillId
  , skillCardCode :: CardCode
  , skillTraits :: HashSet Trait
  , skillOwner :: InvestigatorId
  , skillWeakness :: Bool
  }
  deriving stock (Show, Generic)

instance ToJSON Attrs where
  toJSON = genericToJSON $ aesonOptions $ Just "skill"
  toEncoding = genericToEncoding $ aesonOptions $ Just "skill"

instance FromJSON Attrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "skill"

instance Entity Attrs where
  type EntityId Attrs = SkillId
  toId = skillId
  toSource = SkillSource . skillId
  toTarget = SkillTarget . skillId
  isSource Attrs { skillId } (SkillSource sid) = skillId == sid
  isSource _ _ = False
  isTarget Attrs { skillId } (SkillTarget sid) = skillId == sid
  isTarget _ _ = False

baseAttrs :: InvestigatorId -> SkillId -> CardCode -> Attrs
baseAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          ("missing player card: " <> unpack (unCardCode cardCode))
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unSkillId eid)
  in
    Attrs
      { skillName = pcName
      , skillId = eid
      , skillCardCode = pcCardCode
      , skillTraits = pcTraits
      , skillOwner = iid
      , skillWeakness = False
      }

weaknessAttrs :: InvestigatorId -> SkillId -> CardCode -> Attrs
weaknessAttrs iid eid cardCode =
  let
    MkPlayerCard {..} =
      fromJustNote
          "missing weakness card"
          (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unSkillId eid)
  in
    Attrs
      { skillName = pcName
      , skillId = eid
      , skillCardCode = pcCardCode
      , skillTraits = pcTraits
      , skillOwner = iid
      , skillWeakness = True
      }

instance HasActions env Attrs where
  getActions _ _ _ = pure []

instance (SkillRunner env) => RunMessage env Attrs where
  runMessage _ a = pure a
