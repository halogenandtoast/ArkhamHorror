module Arkham.Types.Effect
  ( module Arkham.Types.Effect
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Effects
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window (Window)

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

createTokenEffect
  :: MonadRandom m
  => EffectMetadata Message
  -> Source
  -> Token
  -> m (EffectId, Effect)
createTokenEffect effectMetadata source token = do
  eid <- getRandom
  pure (eid, buildTokenEffect eid effectMetadata source token)

createPayForAbilityEffect
  :: MonadRandom m
  => Ability
  -> Source
  -> Target
  -> [Window]
  -> m (EffectId, Effect)
createPayForAbilityEffect ability source target windows = do
  eid <- getRandom
  pure (eid, buildPayForAbilityEffect eid ability source target windows)

data Effect
  = OnTheLam' OnTheLam
  | MindOverMatter' MindOverMatter
  | Deduction' Deduction
  | Shrivelling' Shrivelling
  | BlindingLight' BlindingLight
  | MindWipe1' MindWipe1
  | BlindingLight2' BlindingLight2
  | BaseballBat' BaseballBat
  | Lucky' Lucky
  | Lucky2' Lucky2
  | WillToSurvive3' WillToSurvive3
  | SureGamble3' SureGamble3
  | ArkhamWoodsTwistingPaths' ArkhamWoodsTwistingPaths
  | RiteOfSeeking' RiteOfSeeking
  | BindMonster2' BindMonster2
  | PushedIntoTheBeyond' PushedIntoTheBeyond
  | ArcaneBarrier' ArcaneBarrier
  | SongOfTheDead2' SongOfTheDead2
  | FireExtinguisher1' FireExtinguisher1
  | Deduction2' Deduction2
  | ExposeWeakness1' ExposeWeakness1
  | QuickThinking' QuickThinking
  | LuckyDice2' LuckyDice2
  | RiteOfSeeking4' RiteOfSeeking4
  | UndimensionedAndUnseenTabletToken' UndimensionedAndUnseenTabletToken
  | TenAcreMeadow_246' TenAcreMeadow_246
  | AChanceEncounter' AChanceEncounter
  | MinhThiPhan' MinhThiPhan
  | WilliamYorick' WilliamYorick
  | ThePaintedWorld' ThePaintedWorld
  | Improvisation' Improvisation
  | LetMeHandleThis' LetMeHandleThis
  | Fieldwork' Fieldwork
  | SleightOfHand' SleightOfHand
  | Lockpicks1' Lockpicks1
  | AlchemicalTransmutation' AlchemicalTransmutation
  | UncageTheSoul' UncageTheSoul
  | Overzealous' Overzealous
  | TheStrangerACityAflame' TheStrangerACityAflame
  | TheStrangerThePathIsMine' TheStrangerThePathIsMine
  | TheStrangerTheShoresOfHali' TheStrangerTheShoresOfHali
  | TheKingsEdict' TheKingsEdict
  | MrPeabody' MrPeabody
  | CharlesRossEsq' CharlesRossEsq
  | StormOfSpirits' StormOfSpirits
  | FightOrFlight' FightOrFlight
  | MeatCleaver' MeatCleaver
  | MindWipe3' MindWipe3
  | ArkhamWoodsGreatWillow' ArkhamWoodsGreatWillow
  | JeremiahPierce' JeremiahPierce
  | NathanielCho' NathanielCho
  | EighteenDerringer' EighteenDerringer
  | CurseOfTheRougarouTabletToken' CurseOfTheRougarouTabletToken
  | CursedShores' CursedShores
  | Mesmerize' Mesmerize
  | DaisysToteBagAdvanced' DaisysToteBagAdvanced
  | WindowModifierEffect' WindowModifierEffect
  | PayForAbilityEffect' PayForAbilityEffect
  | TokenEffect' TokenEffect
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance
  ( HasSet ClassSymbol env InvestigatorId
  , HasId Difficulty env ()
  , HasCount HorrorCount env InvestigatorId
  , HasCount DoomCount env EnemyId
  , HasCount ClueCount env EnemyId
  , HasId LocationId env InvestigatorId
  , HasId LocationId env AssetId
  )
  => HasModifiersFor env Effect where
  getModifiersFor = genericGetModifiersFor

instance HasAbilities Effect where
  getAbilities = genericGetAbilities

instance
  ( HasQueue env
  , HasId (Maybe OwnerId) env AssetId
  , HasSet Trait env EnemyId
  , HasCostPayment env
  , HasSet Trait env Source
  , HasModifiersFor env ()
  , HasId Difficulty env ()
  , HasCount ClueCount env EnemyId
  , HasSet EnemyId env InvestigatorId
  , HasSet InvestigatorId env ()
  , HasList DiscardedPlayerCard env InvestigatorId
  , HasCount ActionRemainingCount env InvestigatorId
  , HasSet ClassSymbol env InvestigatorId
  , Query EnemyMatcher env
  , HasId LeadInvestigatorId env ()
  , HasId LocationId env InvestigatorId
  , HasId LocationId env AssetId
  , HasSet FarthestLocationId env (InvestigatorId, LocationMatcher)
  )
  => RunMessage env Effect where
  runMessage = genericRunMessage

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
  , ("01060", Shrivelling' . shrivelling)
  , ("01066", BlindingLight' . blindingLight)
  , ("01068", MindWipe1' . mindWipe1)
  , ("01069", BlindingLight2' . blindingLight2)
  , ("01074", BaseballBat' . baseballBat)
  , ("01080", Lucky' . lucky)
  , ("01084", Lucky2' . lucky2)
  , ("01085", WillToSurvive3' . willToSurvive3)
  , ("01088", SureGamble3' . sureGamble3)
  , ("01151", ArkhamWoodsTwistingPaths' . arkhamWoodsTwistingPaths)
  , ("02028", RiteOfSeeking' . riteOfSeeking)
  , ("02031", BindMonster2' . bindMonster2)
  , ("02100", PushedIntoTheBeyond' . pushedIntoTheBeyond)
  , ("02102", ArcaneBarrier' . arcaneBarrier)
  , ("02112", SongOfTheDead2' . songOfTheDead2)
  , ("02114", FireExtinguisher1' . fireExtinguisher1)
  , ("02150", Deduction2' . deduction2)
  , ("02228", ExposeWeakness1' . exposeWeakness1)
  , ("02229", QuickThinking' . quickThinking)
  , ("02230", LuckyDice2' . luckyDice2)
  , ("02233", RiteOfSeeking4' . riteOfSeeking4)
  , ( "02236"
    , UndimensionedAndUnseenTabletToken' . undimensionedAndUnseenTabletToken
    )
  , ("02246", TenAcreMeadow_246' . tenAcreMeadow_246)
  , ("02270", AChanceEncounter' . aChanceEncounter)
  , ("03002", MinhThiPhan' . minhThiPhan)
  , ("03005", WilliamYorick' . williamYorick)
  , ("03012", ThePaintedWorld' . thePaintedWorld)
  , ("03018", Improvisation' . improvisation)
  , ("03022", LetMeHandleThis' . letMeHandleThis)
  , ("03024", Fieldwork' . fieldwork)
  , ("03029", SleightOfHand' . sleightOfHand)
  , ("03031", Lockpicks1' . lockpicks1)
  , ("03032", AlchemicalTransmutation' . alchemicalTransmutation)
  , ("03033", UncageTheSoul' . uncageTheSoul)
  , ("03040", Overzealous' . overzealous)
  , ("03047a", TheStrangerACityAflame' . theStrangerACityAflame)
  , ("03047b", TheStrangerThePathIsMine' . theStrangerThePathIsMine)
  , ("03047c", TheStrangerTheShoresOfHali' . theStrangerTheShoresOfHali)
  , ("03100", TheKingsEdict' . theKingsEdict)
  , ("03141", MrPeabody' . mrPeabody)
  , ("03149", CharlesRossEsq' . charlesRossEsq)
  , ("03153", StormOfSpirits' . stormOfSpirits)
  , ("03155", FightOrFlight' . fightOrFlight)
  , ("05114", MeatCleaver' . meatCleaver)
  , ("50008", MindWipe3' . mindWipe3)
  , ("50033", ArkhamWoodsGreatWillow' . arkhamWoodsGreatWillow)
  , ("50044", JeremiahPierce' . jeremiahPierce)
  , ("60101", NathanielCho' . nathanielCho)
  , ("60505", EighteenDerringer' . eighteenDerringer)
  , ("81001", CurseOfTheRougarouTabletToken' . curseOfTheRougarouTabletToken)
  , ("81007", CursedShores' . cursedShores)
  , ("82035", Mesmerize' . mesmerize)
  , ("90002", DaisysToteBagAdvanced' . daisysToteBagAdvanced)
  ]

buildTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildTokenValueEffect eid n source = buildWindowModifierEffect
  eid
  (EffectModifiers [Modifier source $ TokenValueModifier n])
  EffectSkillTestWindow
  source

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

buildTokenEffect
  :: EffectId -> EffectMetadata Message -> Source -> Token -> Effect
buildTokenEffect eid metadata source token =
  TokenEffect' $ tokenEffect eid metadata source token

buildPayForAbilityEffect
  :: EffectId -> Ability -> Source -> Target -> [Window] -> Effect
buildPayForAbilityEffect eid ability source target windows =
  PayForAbilityEffect' $ payForAbilityEffect eid ability source target windows
