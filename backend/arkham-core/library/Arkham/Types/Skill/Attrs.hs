{-# LANGUAGE TemplateHaskell #-}

module Arkham.Types.Skill.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.CampaignId
import Arkham.Types.Card
import Arkham.Types.Card.Cost
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EncounterSet (EncounterSet)
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Prey
import Arkham.Types.Query
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Stats (Stats)
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TreacheryId
import Arkham.Types.Window


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
