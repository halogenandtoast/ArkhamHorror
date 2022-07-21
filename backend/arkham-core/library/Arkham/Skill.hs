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
lookupSkill cardCode =
  fromJustNote ("Unknown skill: " <> show cardCode) $ lookup cardCode allSkills

instance FromJSON Skill where
  parseJSON v = flip (withObject "Skill") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      "01025" -> Skill . ViciousBlow <$> parseJSON v
      "01039" -> Skill . Deduction <$> parseJSON v
      "01053" -> Skill . Opportunist <$> parseJSON v
      "01067" -> Skill . Fearless <$> parseJSON v
      "01081" -> Skill . SurvivalInstinct <$> parseJSON v
      "01089" -> Skill . Guts <$> parseJSON v
      "01090" -> Skill . Perception <$> parseJSON v
      "01091" -> Skill . Overpower <$> parseJSON v
      "01092" -> Skill . ManualDexterity <$> parseJSON v
      "01093" -> Skill . UnexpectedCourage <$> parseJSON v
      "02026" -> Skill . DoubleOrNothing <$> parseJSON v
      "02150" -> Skill . Deduction2 <$> parseJSON v
      "02190" -> Skill . Defiance <$> parseJSON v
      "02192" -> Skill . RiseToTheOccasion <$> parseJSON v
      "02227" -> Skill . InquiringMind <$> parseJSON v
      "02229" -> Skill . QuickThinking <$> parseJSON v
      "02231" -> Skill . Opportunist2 <$> parseJSON v
      "02235" -> Skill . SurvivalInstinct2 <$> parseJSON v
      "02260" -> Skill . Leadership <$> parseJSON v
      "02268" -> Skill . Fearless2 <$> parseJSON v
      "02271" -> Skill . StrokeOfLuck2 <$> parseJSON v
      "02299" -> Skill . ViciousBlow2 <$> parseJSON v
      "03007" -> Skill . TheHomeFront <$> parseJSON v
      "03039" -> Skill . Resourceful <$> parseJSON v
      "03116" -> Skill . SayYourPrayers <$> parseJSON v
      "03117" -> Skill . DesperateSearch <$> parseJSON v
      "03118" -> Skill . RecklessAssault <$> parseJSON v
      "03119" -> Skill . RunForYourLife <$> parseJSON v
      "03228" -> Skill . InspiringPresence <$> parseJSON v
      "03231" -> Skill . Eureka <$> parseJSON v
      "03233" -> Skill . WatchThis <$> parseJSON v
      "03235" -> Skill . TorrentOfPower <$> parseJSON v
      "03272" -> Skill . NotWithoutAFight <$> parseJSON v
      "03312" -> Skill . SealOfTheElderSign5 <$> parseJSON v
      "04036" -> Skill . LastChance <$> parseJSON v
      "04153" -> Skill . TrueUnderstanding <$> parseJSON v
      "04201" -> Skill . TakeHeart <$> parseJSON v
      "60502" -> Skill . NeitherRainNorSnow <$> parseJSON v
      "60526" -> Skill . UnexpectedCourage2 <$> parseJSON v
      _ -> error "Unknown skill"

allSkills :: HashMap CardCode (InvestigatorId -> SkillId -> Skill)
allSkills = mapFromList $ map (cbCardCode &&& (curry . cbCardBuilder))
  [ Skill <$> viciousBlow
  , Skill <$> deduction
  , Skill <$> opportunist
  , Skill <$> fearless
  , Skill <$> survivalInstinct
  , Skill <$> guts
  , Skill <$> perception
  , Skill <$> overpower
  , Skill <$> manualDexterity
  , Skill <$> unexpectedCourage
  , Skill <$> doubleOrNothing
  , Skill <$> deduction2
  , Skill <$> defiance
  , Skill <$> riseToTheOccasion
  , Skill <$> inquiringMind
  , Skill <$> quickThinking
  , Skill <$> opportunist2
  , Skill <$> survivalInstinct2
  , Skill <$> leadership
  , Skill <$> fearless2
  , Skill <$> strokeOfLuck2
  , Skill <$> viciousBlow2
  , Skill <$> theHomeFront
  , Skill <$> resourceful
  , Skill <$> sayYourPrayers
  , Skill <$> desperateSearch
  , Skill <$> recklessAssault
  , Skill <$> runForYourLife
  , Skill <$> inspiringPresence
  , Skill <$> eureka
  , Skill <$> watchThis
  , Skill <$> torrentOfPower
  , Skill <$> notWithoutAFight
  , Skill <$> sealOfTheElderSign5
  , Skill <$> lastChance
  , Skill <$> trueUnderstanding
  , Skill <$> takeHeart
  , Skill <$> neitherRainNorSnow
  , Skill <$> unexpectedCourage2
  ]
