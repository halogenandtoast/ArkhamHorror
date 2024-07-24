{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Skill where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Id
import Arkham.Skill.Runner
import Arkham.Skill.Skills

createSkill :: IsCard a => a -> InvestigatorId -> SkillId -> Skill
createSkill a iid sId =
  let this = lookupSkill (toCardCode a) iid sId (toCardId a)
   in overAttrs (\attrs -> attrs {skillCustomizations = customizations}) this
 where
  customizations = case toCard a of
    PlayerCard pc -> pcCustomizations pc
    _ -> mempty

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
    [ -- Night of the Zealot
      --- guardian [notz]
      SomeSkillCard viciousBlow
    , --- seeker [notz]
      SomeSkillCard deduction
    , --- rogue [notz]
      SomeSkillCard opportunist
    , --- mystic [notz]
      SomeSkillCard fearless
    , --- survivor [notz]
      SomeSkillCard survivalInstinct
    , --- neutral [notz]
      SomeSkillCard guts
    , SomeSkillCard perception
    , SomeSkillCard overpower
    , SomeSkillCard manualDexterity
    , SomeSkillCard unexpectedCourage
    , -- The Dunwich Legacy
      --- rogue [tdl]
      SomeSkillCard doubleOrNothing
    , --- seeker [tdl]
      SomeSkillCard deduction2
    , --- mystic [tdl]
      SomeSkillCard defiance
    , --- survivor [tdl]
      SomeSkillCard riseToTheOccasion
    , -- Undimensioned and Unseen
      --- seeker [uau]
      SomeSkillCard inquiringMind
    , --- rogue [uau]
      SomeSkillCard quickThinking
    , SomeSkillCard opportunist2
    , --- survivor [uau]
      SomeSkillCard survivalInstinct2
    , -- Where Doom Awaits
      --- guardian [wda]
      SomeSkillCard leadership
    , --- mystic [wda]
      SomeSkillCard fearless2
    , --- survivor [wda]
      SomeSkillCard strokeOfLuck2
    , -- Lost in Time and Space
      --- guardian [litas]
      SomeSkillCard viciousBlow2
    , -- The Path to Carcosa
      --- signature [tpc]
      SomeSkillCard theHomeFront
    , --- survivor [tpc]
      SomeSkillCard resourceful
    , -- Echoes of the Past
      --- neutral [eotp]
      SomeSkillCard sayYourPrayers
    , SomeSkillCard desperateSearch
    , SomeSkillCard recklessAssault
    , SomeSkillCard runForYourLife
    , -- The Pallid Mask
      --- guardian [tpm]
      SomeSkillCard inspiringPresence
    , --- seeker [tpm]
      SomeSkillCard eureka
    , --- rogue [tpm]
      SomeSkillCard watchThis
    , --- mystic [tpm]
      SomeSkillCard torrentOfPower
    , -- Black Stars Rise
      --- survivor [bsr]
      SomeSkillCard notWithoutAFight
    , -- Dim Carcosa
      --- mystic [dca]
      SomeSkillCard sealOfTheElderSign5
    , -- The Forgotten Age
      --- survivor [tfa]
      SomeSkillCard lastChance
    , -- Threads of Fate
      --- survivor [tof]
      SomeSkillCard stunningBlow
    , -- The Boundary Beyond
      --- guardian [tbb]
      SomeSkillCard takeTheInitiative
    , --- seeker [tbb]
      SomeSkillCard trueUnderstanding
    , --- rogue [tbb]
      SomeSkillCard hatchetMan
    , --- mystic [tbb]
      SomeSkillCard enraptured
    , -- Heart of the Elders
      --- guardian [hote]
      SomeSkillCard intrepid
    , --- mystic [hote]
      SomeSkillCard defiance2
    , --- survivor [hote]
      SomeSkillCard takeHeart
    , -- Shattered Aeons
      --- rogue [sha]
      SomeSkillCard allIn5
    , -- The Circle Undone
      --- guardian [tcu]
      SomeSkillCard steadfast
    , --- seeker [tcu]
      SomeSkillCard curiosity
    , --- rogue [tcu]
      SomeSkillCard cunning
    , --- mystic [tcu]
      SomeSkillCard prophesy
    , --- survivor [tcu]
      SomeSkillCard ableBodied
    , -- The Dream Eaters
      -- The Search for Kadath
      --- guardian [sfk]
      SomeSkillCard daring
    , --- seeker [sfk]
      SomeSkillCard essenceOfTheDream
    , --- rogue [sfk]
      SomeSkillCard momentum1
    , -- A Thousand Shapes of Horror
      --- guardian [tsh]
      SomeSkillCard selfSacrifice
    , --- survivor [tsh]
      SomeSkillCard bruteForce1
    , -- Dark Side of the Moon
      --- rogue [dsm]
      SomeSkillCard threeAces1
    , --- survivor [dsm]
      SomeSkillCard sharpVision1
    , -- Point of No Return
      --- guardian [pnr]
      SomeSkillCard leadership2
    , --- rogue [pnr]
      SomeSkillCard daredevil2
    , --- survivor [pnr]
      SomeSkillCard expeditiousRetreat1
    , -- Where the Gods Dwell
      --- seeker [wgd]
      SomeSkillCard surprisingFind1
    , -- Weaver of the Cosmos
      --- seeker [woc]
      SomeSkillCard theEyeOfTruth5
    , --- seeker/bonded/theEyeOfTruth5 [woc]
      SomeSkillCard dreamParasite
    , -- The Innsmouth Conspiracy
      --- signature [tic]
      SomeSkillCard whispersFromTheDeep
    , --- seeker [tic]
      SomeSkillCard planOfAction
    , --- mystic [tic]
      SomeSkillCard promiseOfPower
    , --- survivor [tic]
      SomeSkillCard predestined
    , SomeSkillCard beloved
    , -- In Too Deep
      --- rogue [itd]
      SomeSkillCard skeptic1
    , -- Horror in High Gear
      --- survivor [hhg]
      SomeSkillCard unrelenting1
    , SomeSkillCard signumCrucis2
    , -- A Light in the Fog
      --- seeker [lif]
      SomeSkillCard fey1
    , -- Into the Maelstrom
      --- rogue  [itm]
      SomeSkillCard justifyTheMeans3
    , -- Edge of the Earth
      --- guardian [eote]
      SomeSkillCard defensiveStance1
    , --- seeker [eote]
      SomeSkillCard surveyTheArea1
    , --- rogue [eote]
      SomeSkillCard savant1
    , --- mystic [eote]
      SomeSkillCard occultTheory1
    , --- survivor [eote]
      SomeSkillCard strengthInNumbers1
    , SomeSkillCard dauntlessSpirit1
    , -- The Scarlet Keys
      --- signature [tsk]
      SomeSkillCard asYouWish
    , SomeSkillCard onTheMend
    , --- guardian [tsk]
      SomeSkillCard fightingLessons
    , SomeSkillCard helpingHand
    , --- seeker [tsk]
      SomeSkillCard analysis
    , --- rogue [tsk]
      SomeSkillCard calculatedRisk
    , --- mystic [tsk]
      SomeSkillCard ghastlyPossession1
    , --- survivor [tsk]
      SomeSkillCard grizzled
    , SomeSkillCard gumption1
    , -- Return to the Dunwich Legacy
      --- survivor [rtdwl]
      SomeSkillCard riseToTheOccasion3
    , -- Nathanial Cho
      --- guardian [nat]
      SomeSkillCard overpower2
    , -- Harvey Walters
      -- seeker [har]
      SomeSkillCard perception2
    , -- Winifred Habbamock
      --- signature [win]
      SomeSkillCard anythingYouCanDoBetter
    , --- signature weakness [win]
      SomeSkillCard arrogance
    , --- basic weakness [win]
      SomeSkillCard reckless
    , --- rogue [win]
      SomeSkillCard nimble
    , SomeSkillCard daredevil
    , SomeSkillCard manualDexterity2
    , SomeSkillCard copycat3
    , -- Jacqueline Fine
      --- mystic [jac]
      SomeSkillCard prescient
    , SomeSkillCard guts2
    , -- Stella Clark
      --- signature [ste]
      SomeSkillCard neitherRainNorSnow
    , --- survivor [ste]
      SomeSkillCard unexpectedCourage2
    , -- The Deep Gate
      --- signature [tdg]
      SomeSkillCard nauticalProwess
    , --- signature weakness [tdg]
      SomeSkillCard dreamsOfTheDeepTheDeepGate
    ]
