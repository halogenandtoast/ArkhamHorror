{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Skill.Attrs where

import Arkham.Json
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard (playerCardAttrs)
import qualified Arkham.Types.Card.PlayerCard.Attrs as PlayerCard
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Skill.Runner
import Arkham.Types.SkillId
import Arkham.Types.Trait
import ClassyPrelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Safe (fromJustNote)

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
    PlayerCard.Attrs {..} =
      playerCardAttrs
        . fromJustNote
            ("missing player card: " <> unpack (unCardCode cardCode))
            (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unSkillId eid)
  in
    Attrs
      { skillName = pcName
      , skillId = eid
      , skillCardCode = pcCardCode
      , skillTraits = HashSet.fromList pcTraits
      , skillOwner = iid
      , skillWeakness = False
      }

weaknessAttrs :: InvestigatorId -> SkillId -> CardCode -> Attrs
weaknessAttrs iid eid cardCode =
  let
    PlayerCard.Attrs {..} =
      playerCardAttrs
        . fromJustNote
            "missing weakness card"
            (HashMap.lookup cardCode allPlayerCards)
        $ CardId (unSkillId eid)
  in
    Attrs
      { skillName = pcName
      , skillId = eid
      , skillCardCode = pcCardCode
      , skillTraits = HashSet.fromList pcTraits
      , skillOwner = iid
      , skillWeakness = True
      }

instance HasActions env investigator Attrs where
  getActions _ _ _ = pure []

instance (SkillRunner env) => RunMessage env Attrs where
  runMessage _ a = pure a
