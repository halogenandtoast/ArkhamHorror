{-# LANGUAGE TemplateHaskell #-}
module Arkham.Effect (
  module Arkham.Effect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Attrs
import Arkham.Effect.Effects
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Game.Helpers
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait
import Arkham.Window (Window)
import Data.Aeson.TH

$(buildEntity "Effect")

$(deriveJSON defaultOptions ''Effect)

createEffect ::
  MonadRandom m =>
  CardCode ->
  Maybe (EffectMetadata Window Message) ->
  Source ->
  Target ->
  m (EffectId, Effect)
createEffect cardCode meffectMetadata source target = do
  eid <- getRandom
  pure (eid, lookupEffect cardCode eid meffectMetadata source target)

createTokenValueEffect ::
  MonadRandom m => Int -> Source -> Target -> m (EffectId, Effect)
createTokenValueEffect n source target = do
  eid <- getRandom
  pure (eid, buildTokenValueEffect eid n source target)

createWindowModifierEffect ::
  MonadRandom m =>
  EffectWindow ->
  EffectMetadata Window Message ->
  Source ->
  Target ->
  m (EffectId, Effect)
createWindowModifierEffect effectWindow effectMetadata source target = do
  eid <- getRandom
  pure
    ( eid
    , buildWindowModifierEffect eid effectMetadata effectWindow source target
    )

createTokenEffect ::
  MonadRandom m =>
  EffectMetadata Window Message ->
  Source ->
  Token ->
  m (EffectId, Effect)
createTokenEffect effectMetadata source token = do
  eid <- getRandom
  pure (eid, buildTokenEffect eid effectMetadata source token)

createPayForAbilityEffect ::
  MonadRandom m =>
  Ability ->
  Source ->
  Target ->
  [Window] ->
  m (EffectId, Effect)
createPayForAbilityEffect ability source target windows' = do
  eid <- getRandom
  pure (eid, buildPayForAbilityEffect eid ability source target windows')

instance
  ( HasSet ClassSymbol env InvestigatorId
  , HasId Difficulty env ()
  , HasCount HorrorCount env InvestigatorId
  , HasCount DoomCount env EnemyId
  , HasCount ClueCount env EnemyId
  , HasId LocationId env InvestigatorId
  , HasId LocationId env AssetId
  ) =>
  HasModifiersFor env Effect
  where
  getModifiersFor = $(entityF2 "Effect" "getModifiersFor")

instance HasAbilities Effect where
  getAbilities = $(entityF "Effect" "getAbilities")

instance
  ( HasQueue env
  , HasCostPayment env
  , HasId Difficulty env ()
  , HasCount ClueCount env EnemyId
  , HasSet ClassSymbol env InvestigatorId
  , HasId LeadInvestigatorId env ()
  , CanCheckPlayable env
  ) =>
  RunMessage env Effect
  where
  runMessage = $(entityRunMessage "Effect")

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Effect" "toAttrs")

instance TargetEntity Effect where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Effect where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasSet Trait env Effect where
  getSet = const (pure mempty)

lookupEffect ::
  CardCode ->
  EffectId ->
  Maybe (EffectMetadata Window Message) ->
  Source ->
  Target ->
  Effect
lookupEffect cardCode eid mmetadata source target =
  effect
    (eid, mmetadata, source, target)
 where
  effect =
    findWithDefault
      (error $ "Unknown effect: " <> show cardCode)
      cardCode
      allEffects

allEffects :: HashMap CardCode (EffectArgs -> Effect)
allEffects =
  mapFromList
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
    , ("02009", RexsCurse' . rexsCurse)
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
    ,
      ( "02236"
      , UndimensionedAndUnseenTabletToken' . undimensionedAndUnseenTabletToken
      )
    , ("02246", TenAcreMeadow_246' . tenAcreMeadow_246)
    , ("02270", AChanceEncounter' . aChanceEncounter)
    , ("02323", YogSothoth' . yogSothoth)
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
    , ("03158", CallingInFavors' . callingInFavors)
    , ("03209", Montmartre209' . montmartre209)
    , ("03215", PereLachaiseCemetery' . pereLachaiseCemetery)
    , ("03218", LeMarais218' . leMarais218)
    , ("05114", MeatCleaver' . meatCleaver)
    , ("50008", MindWipe3' . mindWipe3)
    , ("50033", ArkhamWoodsGreatWillow' . arkhamWoodsGreatWillow)
    , ("50044", JeremiahPierce' . jeremiahPierce)
    , ("60101", NathanielCho' . nathanielCho)
    , ("60103", TommyMalloy' . tommyMalloy)
    , ("60305", Lockpicks' . lockpicks)
    , ("60505", EighteenDerringer' . eighteenDerringer)
    , ("81001", CurseOfTheRougarouTabletToken' . curseOfTheRougarouTabletToken)
    , ("81007", CursedShores' . cursedShores)
    , ("82035", Mesmerize' . mesmerize)
    , ("90002", DaisysToteBagAdvanced' . daisysToteBagAdvanced)
    ]

buildTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildTokenValueEffect eid n source =
  buildWindowModifierEffect
    eid
    (EffectModifiers [Modifier source $ TokenValueModifier n])
    EffectSkillTestWindow
    source

buildWindowModifierEffect ::
  EffectId ->
  EffectMetadata Window Message ->
  EffectWindow ->
  Source ->
  Target ->
  Effect
buildWindowModifierEffect eid metadata effectWindow source target =
  WindowModifierEffect' $
    windowModifierEffect eid metadata effectWindow source target

buildTokenEffect ::
  EffectId -> EffectMetadata Window Message -> Source -> Token -> Effect
buildTokenEffect eid metadata source token =
  TokenEffect' $ tokenEffect eid metadata source token

buildPayForAbilityEffect ::
  EffectId -> Ability -> Source -> Target -> [Window] -> Effect
buildPayForAbilityEffect eid ability source target windows' =
  PayForAbilityEffect' $ payForAbilityEffect eid ability source target windows'
