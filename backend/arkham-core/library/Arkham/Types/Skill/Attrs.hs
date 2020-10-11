{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Attrs where

import Arkham.Import

import Arkham.Types.Skill.Runner
import Arkham.Types.Trait
import qualified Data.HashMap.Strict as HashMap

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

instance HasActions env investigator Attrs where
  getActions _ _ _ = pure []

instance (SkillRunner env) => RunMessage env Attrs where
  runMessage _ a = pure a
