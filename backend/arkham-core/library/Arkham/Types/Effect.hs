module Arkham.Types.Effect
  ( lookupEffect
  , buildSkillTestEffect
  , buildTokenValueEffect
  , buildPhaseEffect
  , Effect(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Effects
import Arkham.Types.Trait
import Data.Coerce

data Effect
  = OnTheLam' OnTheLam
  | MindOverMatter' MindOverMatter
  | Deduction' Deduction
  | Burglary' Burglary
  | Shrivelling' Shrivelling
  | BlindingLight' BlindingLight
  | MindWipe1' MindWipe1
  | BlindingLight2' BlindingLight2
  | BaseballBat' BaseballBat
  | Lucky' Lucky
  | Lucky2' Lucky2
  | WillToSurvive4' WillToSurvive4
  | SureGamble3' SureGamble3
  | ArkhamWoodsTwistingPaths' ArkhamWoodsTwistingPaths
  | HuntingNightgaunt' HuntingNightgaunt
  | SeekingAnswers' SeekingAnswers
  | RiteOfSeeking' RiteOfSeeking
  | BindMonster2' BindMonster2
  | PushedIntoTheBeyond' PushedIntoTheBeyond
  | ArcaneBarrier' ArcaneBarrier
  | LetMeHandleThis' LetMeHandleThis
  | MindWipe3' MindWipe3
  | JeremiahPierce' JeremiahPierce
  | CurseOfTheRougarouTabletToken' CurseOfTheRougarouTabletToken
  | CursedShores' CursedShores
  | SkillTestEffect' SkillTestEffect
  | PhaseEffect' PhaseEffect
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasModifiersFor env Effect
deriving anyclass instance
  ( HasQueue env
  , HasSet ConnectedLocationId env LocationId
  , HasSet Trait env EnemyId
  )
  => RunMessage env Effect

instance Entity Effect where
  type EntityId Effect = EffectId
  toId = toId . effectAttrs
  toSource = toSource . effectAttrs
  toTarget = toTarget . effectAttrs
  isSource = isSource . effectAttrs
  isTarget = isTarget . effectAttrs

instance HasSet Trait env Effect where
  getSet = const (pure mempty)

lookupEffect
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Message)
  -> Source
  -> Target
  -> Effect
lookupEffect cardCode eid mmetadata source target = effect
  (eid, mmetadata, source, target)
 where
  effect = findWithDefault
    (error $ "Unknown effect: " <> show cardCode)
    cardCode
    allEffects

allEffects :: HashMap CardCode (EffectArgs -> Effect)
allEffects = mapFromList
  [ ("01010", OnTheLam' . onTheLam)
  , ("01036", MindOverMatter' . mindOverMatter)
  , ("01039", Deduction' . deduction)
  , ("01045", Burglary' . burglary)
  , ("01060", Shrivelling' . shrivelling)
  , ("01066", BlindingLight' . blindingLight)
  , ("01068", MindWipe1' . mindWipe1)
  , ("01069", BlindingLight2' . blindingLight2)
  , ("01074", BaseballBat' . baseballBat)
  , ("01080", Lucky' . lucky)
  , ("01084", Lucky2' . lucky2)
  , ("01085", WillToSurvive4' . willToSurvive4)
  , ("01088", SureGamble3' . sureGamble3)
  , ("01151", ArkhamWoodsTwistingPaths' . arkhamWoodsTwistingPaths)
  , ("01172", HuntingNightgaunt' . huntingNightgaunt)
  , ("02023", SeekingAnswers' . seekingAnswers)
  , ("02028", RiteOfSeeking' . riteOfSeeking)
  , ("02031", BindMonster2' . bindMonster2)
  , ("02100", PushedIntoTheBeyond' . pushedIntoTheBeyond)
  , ("02102", ArcaneBarrier' . arcaneBarrier)
  , ("03022", LetMeHandleThis' . letMeHandleThis)
  , ("50008", MindWipe3' . mindWipe3)
  , ("50044", JeremiahPierce' . jeremiahPierce)
  , ("81001", CurseOfTheRougarouTabletToken' . curseOfTheRougarouTabletToken)
  , ("81007", CursedShores' . cursedShores)
  ]

buildSkillTestEffect
  :: EffectId -> EffectMetadata Message -> Source -> Target -> Effect
buildSkillTestEffect eid metadata source target =
  SkillTestEffect' $ skillTestEffect eid metadata source target

buildTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildTokenValueEffect eid n source target = SkillTestEffect' $ skillTestEffect
  eid
  (EffectModifiers [Modifier source $ TokenValueModifier n])
  source
  target

buildPhaseEffect
  :: EffectId -> EffectMetadata Message -> Source -> Target -> Effect
buildPhaseEffect eid metadata source target =
  PhaseEffect' $ phaseEffect eid metadata source target

effectAttrs :: Effect -> Attrs
effectAttrs = \case
  OnTheLam' attrs -> coerce attrs
  MindOverMatter' attrs -> coerce attrs
  Deduction' attrs -> coerce attrs
  Burglary' attrs -> coerce attrs
  Shrivelling' attrs -> coerce attrs
  BlindingLight' attrs -> coerce attrs
  MindWipe1' attrs -> coerce attrs
  BlindingLight2' attrs -> coerce attrs
  BaseballBat' attrs -> coerce attrs
  Lucky' attrs -> coerce attrs
  Lucky2' attrs -> coerce attrs
  WillToSurvive4' attrs -> coerce attrs
  SureGamble3' attrs -> coerce attrs
  ArkhamWoodsTwistingPaths' attrs -> coerce attrs
  HuntingNightgaunt' attrs -> coerce attrs
  SeekingAnswers' attrs -> coerce attrs
  RiteOfSeeking' attrs -> coerce attrs
  BindMonster2' attrs -> coerce attrs
  PushedIntoTheBeyond' attrs -> coerce attrs
  ArcaneBarrier' attrs -> coerce attrs
  LetMeHandleThis' attrs -> coerce attrs
  MindWipe3' attrs -> coerce attrs
  JeremiahPierce' attrs -> coerce attrs
  CurseOfTheRougarouTabletToken' attrs -> coerce attrs
  CursedShores' attrs -> coerce attrs
  SkillTestEffect' attrs -> coerce attrs
  PhaseEffect' attrs -> coerce attrs
