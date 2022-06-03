module Arkham.Skill.Attrs where

import Arkham.Prelude

import Arkham.Json
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.InvestigatorId
import Arkham.Message
import Arkham.Name
import Arkham.SkillId
import Arkham.Source
import Arkham.Target

type SkillCard a = CardBuilder (InvestigatorId, SkillId) a

class IsSkill a

data SkillAttrs = SkillAttrs
  { skillCardCode :: CardCode
  , skillId :: SkillId
  , skillOwner :: InvestigatorId
  , skillAdditionalCost :: Maybe Cost
  , skillAdditionalPayment :: Maybe Payment
  }
  deriving stock (Show, Eq, Generic)

additionalCostL :: Lens' SkillAttrs (Maybe Cost)
additionalCostL = lens skillAdditionalCost $ \m x -> m { skillAdditionalCost = x }

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
    }
  }

instance RunMessage SkillAttrs where
  runMessage msg a = case msg of
    UseCardAbility _ (isSource a -> True) _ (-1) payment ->
      pure $ a { skillAdditionalPayment = Just payment }
    _ -> pure a
