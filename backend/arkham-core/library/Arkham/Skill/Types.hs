module Arkham.Skill.Types where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.HasModifiersFor
import Arkham.Classes.RunMessage.Internal
import Arkham.Cost
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Skill.Cards (allPlayerSkillCards)
import Arkham.Source
import Arkham.Strategy
import Arkham.Target
import Arkham.Trait
import Data.Typeable
import GHC.Records

class
  ( Typeable a
  , ToJSON a
  , FromJSON a
  , Eq a
  , Show a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ SkillId
  , EntityAttrs a ~ SkillAttrs
  ) =>
  IsSkill a

type SkillCard a = CardBuilder (InvestigatorId, SkillId) a

data instance Field Skill :: Type -> Type where
  SkillTraits :: Field Skill (Set Trait)
  SkillCard :: Field Skill Card
  SkillOwner :: Field Skill InvestigatorId

data SkillAttrs = SkillAttrs
  { skillCardCode :: CardCode
  , skillCardId :: CardId
  , skillId :: SkillId
  , skillOwner :: InvestigatorId
  , skillAdditionalCost :: Maybe Cost
  , skillAdditionalPayment :: Maybe Payment
  , skillAfterPlay :: AfterPlayStrategy
  , skillPlacement :: Placement
  }
  deriving stock (Show, Eq, Generic)

instance HasField "controller" SkillAttrs InvestigatorId where
  getField = skillOwner

additionalCostL :: Lens' SkillAttrs (Maybe Cost)
additionalCostL = lens skillAdditionalCost $ \m x -> m {skillAdditionalCost = x}

afterPlayL :: Lens' SkillAttrs AfterPlayStrategy
afterPlayL = lens skillAfterPlay $ \m x -> m {skillAfterPlay = x}

allSkillCards :: Map CardCode CardDef
allSkillCards = allPlayerSkillCards

instance HasCardCode SkillAttrs where
  toCardCode = skillCardCode

instance HasCardDef SkillAttrs where
  toCardDef a = case lookup (skillCardCode a) allSkillCards of
    Just def -> def
    Nothing -> error $ "missing card def for skill " <> show (skillCardCode a)

instance IsCard SkillAttrs where
  toCard = defaultToCard
  toCardId = skillCardId
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
  overAttrs f = f

instance Named SkillAttrs where
  toName = toName . toCardDef

instance Targetable SkillAttrs where
  toTarget = SkillTarget . skillId
  isTarget SkillAttrs {skillId} (SkillTarget sid) = skillId == sid
  isTarget _ _ = False

instance Sourceable SkillAttrs where
  toSource = SkillSource . skillId
  isSource SkillAttrs {skillId} (SkillSource sid) = skillId == sid
  isSource _ _ = False

skillWith
  :: (SkillAttrs -> a)
  -> CardDef
  -> (SkillAttrs -> SkillAttrs)
  -> CardBuilder (InvestigatorId, SkillId) a
skillWith f cardDef g = skill (f . g) cardDef

skill
  :: (SkillAttrs -> a) -> CardDef -> CardBuilder (InvestigatorId, SkillId) a
skill f cardDef =
  CardBuilder
    { cbCardCode = cdCardCode cardDef
    , cbCardBuilder = \cardId (iid, sid) ->
        f
          $ SkillAttrs
            { skillCardCode = toCardCode cardDef
            , skillCardId = cardId
            , skillId = sid
            , skillOwner = iid
            , skillAdditionalCost = Nothing
            , skillAdditionalPayment = Nothing
            , skillAfterPlay = DiscardThis
            , skillPlacement = Unplaced
            }
    }

data Skill = forall a. IsSkill a => Skill a

instance Eq Skill where
  Skill (a :: a) == Skill (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Skill where
  show (Skill a) = show a

instance ToJSON Skill where
  toJSON (Skill a) = toJSON a

instance HasCardCode Skill where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Skill where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Skill where
  getAbilities (Skill a) = getAbilities a

instance HasModifiersFor Skill where
  getModifiersFor target (Skill a) = getModifiersFor target a

instance Entity Skill where
  type EntityId Skill = SkillId
  type EntityAttrs Skill = SkillAttrs
  toId = toId . toAttrs
  toAttrs (Skill a) = toAttrs a
  overAttrs f (Skill a) = Skill $ overAttrs f a

instance Named Skill where
  toName = toName . toAttrs

instance Targetable Skill where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance Sourceable Skill where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance IsCard Skill where
  toCard = toCard . toAttrs
  toCardId = toCardId . toAttrs
  toCardOwner = toCardOwner . toAttrs

data SomeSkillCard = forall a. IsSkill a => SomeSkillCard (SkillCard a)

liftSomeSkillCard :: (forall a. SkillCard a -> b) -> SomeSkillCard -> b
liftSomeSkillCard f (SomeSkillCard a) = f a

someSkillCardCode :: SomeSkillCard -> CardCode
someSkillCardCode = liftSomeSkillCard cbCardCode
