module Arkham.Skill.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Cost
import Arkham.InvestigatorId
import Arkham.Name
import Arkham.Projection
import Arkham.SkillId
import Arkham.Strategy
import Arkham.Source
import Arkham.Target
import Arkham.Trait

class IsSkill a

type SkillCard a = CardBuilder (InvestigatorId, SkillId) a

data instance Field SkillAttrs :: Type -> Type where
  SkillTraits :: Field SkillAttrs (HashSet Trait)
  SkillCard :: Field SkillAttrs Card

data SkillAttrs = SkillAttrs
  { skillCardCode :: CardCode
  , skillId :: SkillId
  , skillOwner :: InvestigatorId
  , skillAdditionalCost :: Maybe Cost
  , skillAdditionalPayment :: Maybe Payment
  , skillAfterPlay :: AfterPlayStrategy
  }
  deriving stock (Show, Eq, Generic)

additionalCostL :: Lens' SkillAttrs (Maybe Cost)
additionalCostL = lens skillAdditionalCost $ \m x -> m { skillAdditionalCost = x }

afterPlayL :: Lens' SkillAttrs AfterPlayStrategy
afterPlayL = lens skillAfterPlay $ \m x -> m { skillAfterPlay = x }

allSkillCards :: HashMap CardCode CardDef
allSkillCards = allPlayerSkillCards

instance HasCardCode SkillAttrs where
  toCardCode = skillCardCode

instance HasCardDef SkillAttrs where
  toCardDef a = case lookup (skillCardCode a) allSkillCards of
    Just def -> def
    Nothing -> error $ "missing card def for skill " <> show (skillCardCode a)

instance IsCard SkillAttrs where
  toCardId = unSkillId . skillId
  toCardOwner = Just . skillOwner

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

instance Named SkillAttrs where
  toName = toName . toCardDef

instance TargetEntity SkillAttrs where
  toTarget = SkillTarget . skillId
  isTarget SkillAttrs { skillId } (SkillTarget sid) = skillId == sid
  isTarget _ _ = False

instance SourceEntity SkillAttrs where
  toSource = SkillSource . skillId
  isSource SkillAttrs { skillId } (SkillSource sid) = skillId == sid
  isSource _ _ = False

skillWith
  :: (SkillAttrs -> a) -> CardDef -> (SkillAttrs -> SkillAttrs) -> CardBuilder (InvestigatorId, SkillId) a
skillWith f cardDef g = skill (f . g) cardDef

skill
  :: (SkillAttrs -> a) -> CardDef -> CardBuilder (InvestigatorId, SkillId) a
skill f cardDef = CardBuilder
  { cbCardCode = cdCardCode cardDef
  , cbCardBuilder = \(iid, sid) -> f $ SkillAttrs
    { skillCardCode = toCardCode cardDef
    , skillId = sid
    , skillOwner = iid
    , skillAdditionalCost = Nothing
    , skillAdditionalPayment = Nothing
    , skillAfterPlay = DiscardThis
    }
  }
