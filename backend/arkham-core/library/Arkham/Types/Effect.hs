module Arkham.Types.Effect
  ( module Arkham.Types.Effect
  ) where


import Arkham.Types.Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Effects
import Arkham.Types.Trait

createEffect
  :: MonadRandom m
  => CardCode
  -> Maybe (EffectMetadata Message)
  -> Source
  -> Target
  -> m (EffectId, Effect)
createEffect cardCode meffectMetadata source target = do
  eid <- getRandom
  pure (eid, lookupEffect cardCode eid meffectMetadata source target)

createTokenValueEffect
  :: MonadRandom m => Int -> Source -> Target -> m (EffectId, Effect)
createTokenValueEffect n source target = do
  eid <- getRandom
  pure (eid, buildTokenValueEffect eid n source target)

createWindowModifierEffect
  :: MonadRandom m
  => EffectWindow
  -> EffectMetadata Message
  -> Source
  -> Target
  -> m (EffectId, Effect)
createWindowModifierEffect effectWindow effectMetadata source target = do
  eid <- getRandom
  pure
    ( eid
    , buildWindowModifierEffect eid effectMetadata effectWindow source target
    )

createPayForAbilityEffect
  :: MonadRandom m => Maybe Ability -> Source -> Target -> m (EffectId, Effect)
createPayForAbilityEffect mAbility source target = do
  eid <- getRandom
  pure (eid, buildPayForAbilityEffect eid mAbility source target)

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
  | WindowModifierEffect' WindowModifierEffect
  | PayForAbilityEffect' PayForAbilityEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasModifiersFor env Effect
deriving anyclass instance
  ( HasQueue env
  , HasSet ConnectedLocationId env LocationId
  , HasSet Trait env EnemyId
  , HasCostPayment env
  , HasSet InScenarioInvestigatorId env ()
  , HasSet Trait env Source
  , HasModifiersFor env ()
  , HasList TakenAction env InvestigatorId
  )
  => RunMessage env Effect

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs

instance TargetEntity Effect where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Effect where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

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

buildTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildTokenValueEffect eid n source target = buildWindowModifierEffect
  eid
  (EffectModifiers [Modifier source $ TokenValueModifier n])
  EffectSkillTestWindow
  source
  target

buildWindowModifierEffect
  :: EffectId
  -> EffectMetadata Message
  -> EffectWindow
  -> Source
  -> Target
  -> Effect
buildWindowModifierEffect eid metadata effectWindow source target =
  WindowModifierEffect'
    $ windowModifierEffect eid metadata effectWindow source target

buildPayForAbilityEffect
  :: EffectId -> Maybe Ability -> Source -> Target -> Effect
buildPayForAbilityEffect eid mAbility source target =
  PayForAbilityEffect' $ payForAbilityEffect eid mAbility source target
