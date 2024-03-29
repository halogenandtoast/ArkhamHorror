{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Skill where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Skill.Runner
import Arkham.Skill.Skills

createSkill :: IsCard a => a -> InvestigatorId -> SkillId -> Skill
createSkill a iid sId = lookupSkill (toCardCode a) iid sId (toCardId a)

instance RunMessage Skill where
  runMessage msg (Skill a) = Skill <$> runMessage msg a

lookupSkill :: CardCode -> InvestigatorId -> SkillId -> CardId -> Skill
lookupSkill cardCode = case lookup cardCode allSkills of
  Nothing -> error $ "Unknown skill: " <> show cardCode
  Just (SomeSkillCard a) -> \i s c -> Skill $ cbCardBuilder a c (i, s)

instance FromJSON Skill where
  parseJSON = withObject "Skill" $ \o -> do
    cCode <- o .: "cardCode"
    withSkillCardCode cCode
      $ \(_ :: SkillCard a) -> Skill <$> parseJSON @a (Object o)

withSkillCardCode
  :: CardCode -> (forall a. IsSkill a => SkillCard a -> r) -> r
withSkillCardCode cCode f = case lookup cCode allSkills of
  Nothing -> error $ "Unknown skill: " <> show cCode
  Just (SomeSkillCard a) -> f a

allSkills :: Map CardCode SomeSkillCard
allSkills =
  mapFrom
    someSkillCardCode
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
    , SomeSkillCard intrepid
    , SomeSkillCard defiance2
    , SomeSkillCard takeHeart
    , SomeSkillCard allIn5
    , SomeSkillCard steadfast
    , SomeSkillCard curiosity
    , SomeSkillCard cunning
    , SomeSkillCard daring
    , SomeSkillCard essenceOfTheDream
    , SomeSkillCard momentum1
    , SomeSkillCard selfSacrifice
    , SomeSkillCard bruteForce1
    , SomeSkillCard threeAces1
    , SomeSkillCard sharpVision1
    , SomeSkillCard leadership2
    , SomeSkillCard daredevil2
    , SomeSkillCard expeditiousRetreat1
    , SomeSkillCard surprisingFind1
    , SomeSkillCard theEyeOfTruth5
    , SomeSkillCard dreamParasite
    , SomeSkillCard whispersFromTheDeep
    , SomeSkillCard promiseOfPower
    , SomeSkillCard unrelenting1
    , SomeSkillCard defensiveStance1
    , SomeSkillCard surveyTheArea1
    , SomeSkillCard occultTheory1
    , SomeSkillCard dauntlessSpirit1
    , SomeSkillCard prophesy
    , SomeSkillCard ableBodied
    , SomeSkillCard riseToTheOccasion3
    , SomeSkillCard overpower2
    , SomeSkillCard perception2
    , SomeSkillCard anythingYouCanDoBetter
    , SomeSkillCard arrogance
    , SomeSkillCard reckless
    , SomeSkillCard nimble
    , SomeSkillCard daredevil
    , SomeSkillCard manualDexterity2
    , SomeSkillCard copycat3
    , SomeSkillCard prescient
    , SomeSkillCard guts2
    , SomeSkillCard neitherRainNorSnow
    , SomeSkillCard unexpectedCourage2
    , SomeSkillCard nauticalProwess
    , SomeSkillCard dreamsOfTheDeepTheDeepGate
    ]
