{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Skill where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Skill.Runner
import Arkham.Skill.Skills

createSkill :: IsCard a => a -> InvestigatorId -> Skill
createSkill a iid = lookupSkill (toCardCode a) iid (SkillId $ toCardId a)

instance RunMessage Skill where
  runMessage msg (Skill a) = Skill <$> runMessage msg a

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
  , SomeSkillCard stunningBlow
  , SomeSkillCard takeTheInitiative
  , SomeSkillCard trueUnderstanding
  , SomeSkillCard hatchetMan
  , SomeSkillCard enraptured
  , SomeSkillCard takeHeart
  , SomeSkillCard overpower2
  , SomeSkillCard neitherRainNorSnow
  , SomeSkillCard unexpectedCourage2
  ]
