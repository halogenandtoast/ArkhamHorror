module Arkham.Skill where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Name
import Arkham.Skill.Runner
import Arkham.Skill.Skills
import Data.Typeable

data Skill = forall a. IsSkill a => Skill a

instance Eq Skill where
  (Skill (a :: a)) == (Skill (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Skill where
  show (Skill a) = show a

instance ToJSON Skill where
  toJSON (Skill a) = toJSON a

createSkill :: IsCard a => a -> InvestigatorId -> Skill
createSkill a iid = lookupSkill (toCardCode a) iid (SkillId $ toCardId a)

instance HasCardCode Skill where
  toCardCode = toCardCode . toAttrs

instance HasCardDef Skill where
  toCardDef = toCardDef . toAttrs

instance HasAbilities Skill where
  getAbilities (Skill a) = getAbilities a

instance RunMessage Skill where
  runMessage msg (Skill a) = Skill <$> runMessage msg a

instance HasModifiersFor Skill where
  getModifiersFor source target (Skill a) = getModifiersFor source target a

instance Entity Skill where
  type EntityId Skill = SkillId
  type EntityAttrs Skill = SkillAttrs
  toId = toId . toAttrs
  toAttrs (Skill a) = toAttrs a
  overAttrs f (Skill a) = Skill $ overAttrs f a

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
  toCardOwner = toCardOwner . toAttrs

lookupSkill :: CardCode -> (InvestigatorId -> SkillId -> Skill)
lookupSkill cardCode = case lookup cardCode allSkills of
  Nothing -> error $ "Unknown skill: " <> show cardCode
  Just (SomeSkillCard a) -> \i s -> Skill $ cbCardBuilder a (i, s)

instance FromJSON Skill where
  parseJSON v = flip (withObject "Skill") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    withSkillCardCode cCode $ \(_ :: SkillCard a) -> Skill <$> parseJSON @a v

withSkillCardCode
  :: CardCode
  -> (forall a. IsSkill a => SkillCard a -> r)
  -> r
withSkillCardCode cCode f =
  case lookup cCode allSkills of
    Nothing -> error $ "Unknown skill: " <> show cCode
    Just (SomeSkillCard a) -> f a

data SomeSkillCard = forall a. IsSkill a => SomeSkillCard (SkillCard a)

liftSomeSkillCard :: (forall a. SkillCard a -> b) -> SomeSkillCard -> b
liftSomeSkillCard f (SomeSkillCard a) = f a

someSkillCardCode :: SomeSkillCard -> CardCode
someSkillCardCode = liftSomeSkillCard cbCardCode

allSkills :: HashMap CardCode SomeSkillCard
allSkills = mapFromList $ map (toFst someSkillCardCode)
  [ SomeSkillCard viciousBlow
  , SomeSkillCard deduction
  , SomeSkillCard opportunist
  , SomeSkillCard fearless
  , SomeSkillCard survivalInstinct
  , SomeSkillCard guts
  , SomeSkillCard perception
  , SomeSkillCard overpower
  , SomeSkillCard manualDexterity
  , SomeSkillCard unexpectedCourage
  , SomeSkillCard doubleOrNothing
  , SomeSkillCard deduction2
  , SomeSkillCard defiance
  , SomeSkillCard riseToTheOccasion
  , SomeSkillCard inquiringMind
  , SomeSkillCard quickThinking
  , SomeSkillCard opportunist2
  , SomeSkillCard survivalInstinct2
  , SomeSkillCard leadership
  , SomeSkillCard fearless2
  , SomeSkillCard strokeOfLuck2
  , SomeSkillCard viciousBlow2
  , SomeSkillCard theHomeFront
  , SomeSkillCard resourceful
  , SomeSkillCard sayYourPrayers
  , SomeSkillCard desperateSearch
  , SomeSkillCard recklessAssault
  , SomeSkillCard runForYourLife
  , SomeSkillCard inspiringPresence
  , SomeSkillCard eureka
  , SomeSkillCard watchThis
  , SomeSkillCard torrentOfPower
  , SomeSkillCard notWithoutAFight
  , SomeSkillCard sealOfTheElderSign5
  , SomeSkillCard lastChance
  , SomeSkillCard trueUnderstanding
  , SomeSkillCard takeHeart
  , SomeSkillCard overpower2
  , SomeSkillCard neitherRainNorSnow
  , SomeSkillCard unexpectedCourage2
  ]
