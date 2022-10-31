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
  , SomeSkillCard guts
  , SomeSkillCard overpower
  , SomeSkillCard unexpectedCourage
  ]
