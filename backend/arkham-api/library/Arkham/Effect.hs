{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Effect (
  module Arkham.Effect,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Effect.Effects
import Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Helpers.Modifiers (effectModifiers)
import Arkham.Helpers.Ref (sourceToMaybeCard)
import Arkham.Id
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Window (Window)

-- start importing directly

import Arkham.Act.Acts (
  infiltratingTheLodgeEffect,
  theStrangerACityAflameEffect,
  theStrangerThePathIsMineEffect,
  theStrangerTheShoresOfHaliEffect,
  theYithianRelicEffect,
 )
import Arkham.Agenda.Agendas (
  awakeningEffect,
  lostMemoriesEffect,
  theLoversVIEffect,
  theRedDepthsEffect,
  theWaterRisesEffect,
 )
import Arkham.Asset.Assets (
  aceOfRods1Effect,
  arbiterOfFatesEffect,
  armageddon4Effect,
  armageddonEffect,
  charlesRossEsqEffect,
  crystalPendulumEffect,
  daisysToteBagAdvancedEffect,
  disciplinePrescienceOfFateEffect,
  eldritchTongueEffect,
  empiricalHypothesisEffect,
  eyeOfChaos4Effect,
  eyeOfChaosEffect,
  eyeOfTheDjinnVesselOfGoodAndEvil2Effect,
  eyesOfValusiaTheMothersCunning4Effect,
  fieldworkEffect,
  fireExtinguisher1Effect,
  gildedVoltoEffect,
  grapplingHookEffect,
  graysAnatomyTheDoctorsBible5Effect,
  gregoryGryEffect,
  grislyTotemSeeker3Effect,
  grislyTotemSurvivor3Effect,
  highRoller2Effect,
  ineffableTruth3Effect,
  ineffableTruth5Effect,
  ineffableTruthEffect,
  lockpicks1Effect,
  lockpicksEffect,
  luckyDice2Effect,
  luckyDice3Effect,
  meatCleaverEffect,
  miskatonicArchaeologyFunding4Effect,
  mistsOfRlyeh2Effect,
  mistsOfRlyeh4Effect,
  mistsOfRlyehEffect,
  mrPeabodyEffect,
  pnakoticManuscripts5Effect,
  prismaticSpectaclesLensToTheOtherworld2Effect,
  riteOfSeekingEffect,
  showmanshipEffect,
  shroudOfShadows4Effect,
  shroudOfShadowsEffect,
  sixthSense4Effect,
  sixthSenseEffect,
  steadyHanded1Effect,
  thirtyFiveWinchesterEffect,
  wellConnected3Effect,
  wellConnectedEffect,
  wither4Effect,
  witherEffect,
  yaotl1Effect,
 )
import Arkham.Campaigns.TheInnsmouthConspiracy.Effects.NoAir (noAirEffect)
import Arkham.Enemy.Enemies (
  alejandroVelaEffect,
  boaConstrictorEffect,
  corruptedOrderlyEffect,
  ichtacaScionOfYigEffect,
  jeremiahPierceEffect,
  tommyMalloyEffect,
  yogSothothEffect,
 )
import Arkham.Event.Events (
  aChanceEncounterEffect,
  actOfDesperationEffect,
  atACrossroads1Effect,
  backstab3Effect,
  bideYourTimeEffect,
  bindMonster2Effect,
  blackMarket2Effect,
  callingInFavorsEffect,
  cheapShot2Effect,
  dawnStar1Effect,
  eideticMemory3Effect,
  explosiveWardEffect,
  exposeWeakness1Effect,
  exposeWeakness3Effect,
  fightOrFlightEffect,
  followedEffect,
  getBehindMeEffect,
  hitAndRunEffect,
  illPayYouBackEffect,
  imDoneRunninEffect,
  improvisationEffect,
  marksmanship1Effect,
  mystifyingSongEffect,
  onTheLamAdvancedEffect,
  oneInTheChamberEffect,
  pilfer3Effect,
  sleightOfHandEffect,
  slipAway2Effect,
  snipe1Effect,
  spectralRazor2Effect,
  spectralRazorEffect,
  stormOfSpirits3Effect,
  stormOfSpiritsEffect,
  telescopicSight3Effect,
  thePaintedWorldEffect,
  thirdTimesACharm2Effect,
  tidesOfFateEffect,
  toeToToeEffect,
  vantagePointEffect,
  willToSurviveEffect,
  writtenInTheStarsEffect,
 )
import Arkham.Investigator.Investigators (
  dexterDrakeEffect,
  kymaniJonesEffect,
  nathanielChoEffect,
  pennyWhiteEffect,
  ritaYoungElderSignEffect,
  williamYorickEffect,
  winifredHabbamockEffect,
 )
import Arkham.Location.Locations (
  cursedShoresEffect,
  enchantedWoodsLostWoodsEffect,
  hereticsGravesSpectral_171Effect,
  longWayAroundEffect,
  restaurantEffect,
  tenAcreMeadow_246Effect,
  unvisitedIsleMossCoveredStepsEffect,
  unvisitedIsleStandingStonesEffect,
 )
import Arkham.Skill.Skills (
  copycat3Effect,
  deduction2Effect,
  defiance2Effect,
  defianceEffect,
  fey1Effect,
  hatchetManEffect,
  momentum1Effect,
  prescientEffect,
  surprisingFind1Effect,
  theEyeOfTruth5Effect,
 )
import Arkham.Story.Stories (
  gavriellasFateEffect,
  jeromesFateEffect,
  josefsPlanEffect,
  pennysFateEffect,
  unfinishedBusiness_JEffect,
  valentinosFateEffect,
 )
import Arkham.Treachery.Treacheries (
  chillingPresenceEffect,
  mesmerizeEffect,
  mysteriesOfTheLodgeEffect,
  pushedIntoTheBeyondEffect,
  realityAcid5U21Effect,
  restlessJourneyFallacyEffect,
  restlessJourneyHardshipEffect,
  restlessJourneyLiesEffect,
  tekelili_227Effect,
  theKingsEdictEffect,
  whispersOfHypnosEffect,
 )

noop :: CardCode -> EffectArgs -> NoEffect
noop cCode = NoEffect . uncurry (baseAttrs cCode)

newtype NoEffect = NoEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage NoEffect where
  runMessage msg (NoEffect a) = NoEffect <$> runMessage msg a

createEffect :: MonadRandom m => EffectBuilder -> m (EffectId, Effect)
createEffect builder = do
  eid <- maybe getRandom pure (effectBuilderEffectId builder)
  pure (eid, lookupEffect eid builder)

createChaosTokenValueEffect
  :: (HasGame m, MonadRandom m) => SkillTestId -> Int -> Source -> Target -> m (EffectId, Effect)
createChaosTokenValueEffect sid n source target = do
  eid <- getRandom
  (eid,) <$> buildChaosTokenValueEffect sid eid n source target

createWindowModifierEffect
  :: MonadRandom m
  => EffectWindow
  -> EffectMetadata Window Message
  -> Source
  -> Target
  -> m (EffectId, Effect)
createWindowModifierEffect effectWindow effectMetadata source target = do
  eid <- getRandom
  pure
    ( eid
    , buildWindowModifierEffect eid effectMetadata effectWindow source target
    )

createChaosTokenEffect
  :: MonadRandom m
  => EffectMetadata Window Message
  -> Source
  -> ChaosToken
  -> m (EffectId, Effect)
createChaosTokenEffect effectMetadata source token = do
  eid <- getRandom
  pure (eid, buildChaosTokenEffect eid effectMetadata source token)

createOnRevealChaosTokenEffect
  :: (MonadRandom m, HasGame m)
  => SkillTestId
  -> ChaosTokenMatcher
  -> Source
  -> Target
  -> [Message]
  -> m (EffectId, Effect)
createOnRevealChaosTokenEffect sid matchr source target messages = do
  eid <- getRandom
  mCardId <- toCardId <$$> sourceToMaybeCard source
  let effect = buildOnRevealChaosTokenEffect eid sid matchr source target messages
  pure (eid, updateAttrs effect \a -> a {effectCardId = mCardId})

createEndOfRoundEffect
  :: MonadRandom m
  => Source
  -> [Message]
  -> m (EffectId, Effect)
createEndOfRoundEffect source messages = do
  eid <- getRandom
  pure (eid, buildEndOfRoundEffect eid source messages)

createEndOfTurnEffect
  :: MonadRandom m
  => Source
  -> InvestigatorId
  -> [Message]
  -> m (EffectId, Effect)
createEndOfTurnEffect source iid messages = do
  eid <- getRandom
  pure (eid, buildEndOfTurnEffect eid source iid messages)

createSurgeEffect
  :: (MonadRandom m, Sourceable source, Targetable target, HasGame m)
  => source
  -> target
  -> m (EffectId, Effect)
createSurgeEffect (toSource -> source) (toTarget -> target) = do
  eid <- getRandom
  builder <- makeEffectBuilder "surge" Nothing source target
  pure
    ( eid
    , Effect $ surgeEffect (eid, builder)
    )

instance RunMessage Effect where
  runMessage msg (Effect a) = case msg of
    UseThisAbility {} -> Effect <$> runMessage msg a
    _ -> do
      if effectFinished (toAttrs a)
        then pure $ Effect a
        else Effect <$> runMessage msg a

lookupEffect :: EffectId -> EffectBuilder -> Effect
lookupEffect eid builder =
  case lookup builder.effectBuilderCardCode allEffects of
    Nothing -> error $ "Unknown effect: " <> show builder.effectBuilderCardCode
    Just (SomeEffect f) -> Effect $ f (eid, builder)

buildChaosTokenValueEffect
  :: HasGame m => SkillTestId -> EffectId -> Int -> Source -> Target -> m Effect
buildChaosTokenValueEffect sid eid n source target = do
  ems <- effectModifiers source [ChaosTokenValueModifier n]
  pure $ buildWindowModifierEffect eid ems (EffectSkillTestWindow sid) source target

buildWindowModifierEffect
  :: EffectId
  -> EffectMetadata Window Message
  -> EffectWindow
  -> Source
  -> Target
  -> Effect
buildWindowModifierEffect eid metadata effectWindow source target =
  Effect $ windowModifierEffect' eid metadata effectWindow source target

buildChaosTokenEffect
  :: EffectId -> EffectMetadata Window Message -> Source -> ChaosToken -> Effect
buildChaosTokenEffect eid metadata source token =
  Effect $ chaosTokenEffect' eid metadata source token

buildOnRevealChaosTokenEffect
  :: EffectId -> SkillTestId -> ChaosTokenMatcher -> Source -> Target -> [Message] -> Effect
buildOnRevealChaosTokenEffect eid sid matchr source token msgs =
  Effect $ onRevealChaosTokenEffect' eid sid matchr source token msgs

buildEndOfRoundEffect
  :: EffectId -> Source -> [Message] -> Effect
buildEndOfRoundEffect eid source msgs =
  Effect $ endOfRoundEffect' eid source msgs

buildEndOfTurnEffect
  :: EffectId -> Source -> InvestigatorId -> [Message] -> Effect
buildEndOfTurnEffect eid source iid msgs =
  Effect $ endOfTurnEffect' eid source iid msgs

effectIsForNextGame :: Effect -> Bool
effectIsForNextGame e = case e.window of
  Just EffectSetupWindow -> True
  Just (EffectScenarioSetupWindow {}) -> True
  _ -> False

effectIsForResolution :: Effect -> Bool
effectIsForResolution e = e.window == Just EffectResolutionWindow

instance FromJSON Effect where
  parseJSON = withObject "Effect" $ \o -> do
    cCode <- o .: "cardCode"
    case lookup cCode allEffects of
      Nothing -> error $ "Invalid effect: " <> show cCode
      Just (SomeEffect (_ :: EffectArgs -> a)) ->
        Effect <$> parseJSON @a (Object o)

allEffects :: Map CardCode SomeEffect
allEffects =
  mapFromList
    [ ("01060", SomeEffect $ noop "01060")
    , ("01066", SomeEffect $ noop "01066")
    , ("01069", SomeEffect $ noop "01069")
    , ("01074", SomeEffect $ noop "01074")
    , ("02028", SomeEffect riteOfSeekingEffect)
    , ("02031", SomeEffect bindMonster2Effect)
    , ("02100", SomeEffect pushedIntoTheBeyondEffect)
    , ("02112", SomeEffect $ noop "02112")
    , ("02114", SomeEffect fireExtinguisher1Effect)
    , ("02150", SomeEffect deduction2Effect)
    , ("02190", SomeEffect defianceEffect)
    , ("02228", SomeEffect exposeWeakness1Effect)
    , ("02230", SomeEffect luckyDice2Effect)
    , ("02236", SomeEffect undimensionedAndUnseenTabletToken)
    , ("02246", SomeEffect tenAcreMeadow_246Effect)
    , ("02270", SomeEffect aChanceEncounterEffect)
    , ("02323", SomeEffect yogSothothEffect)
    , ("03005", SomeEffect williamYorickEffect)
    , ("03012", SomeEffect thePaintedWorldEffect)
    , ("03018", SomeEffect improvisationEffect)
    , ("03024", SomeEffect fieldworkEffect)
    , ("03029", SomeEffect sleightOfHandEffect)
    , ("03031", SomeEffect lockpicks1Effect)
    , ("03032", SomeEffect $ noop "03032")
    , ("03033", SomeEffect $ noop "03033")
    , ("03047a", SomeEffect theStrangerACityAflameEffect)
    , ("03047b", SomeEffect theStrangerThePathIsMineEffect)
    , ("03047c", SomeEffect theStrangerTheShoresOfHaliEffect)
    , ("03100", SomeEffect theKingsEdictEffect)
    , ("03141", SomeEffect mrPeabodyEffect)
    , ("03149", SomeEffect charlesRossEsqEffect)
    , ("03153", SomeEffect stormOfSpiritsEffect)
    , ("03155", SomeEffect fightOrFlightEffect)
    , ("03158", SomeEffect callingInFavorsEffect)
    , ("03209", SomeEffect $ noop "03209")
    , ("03254", SomeEffect $ noop "03254")
    , ("03306", SomeEffect eideticMemory3Effect)
    , ("04004", SomeEffect $ noop "04004")
    , ("04029", SomeEffect mistsOfRlyehEffect)
    , ("04035", SomeEffect yaotl1Effect)
    , ("04079", SomeEffect boaConstrictorEffect)
    , ("04104", SomeEffect marksmanship1Effect)
    , ("04108", SomeEffect $ noop "04108")
    , ("04155", SomeEffect hatchetManEffect)
    , ("04156", SomeEffect highRoller2Effect)
    , ("04195", SomeEffect exposeWeakness3Effect)
    , ("04198", SomeEffect defiance2Effect)
    , ("04239", SomeEffect lostMemoriesEffect)
    , ("04271", SomeEffect mistsOfRlyeh4Effect)
    , ("04283", SomeEffect theRedDepthsEffect)
    , ("04306", SomeEffect vantagePointEffect)
    , ("04307", SomeEffect pnakoticManuscripts5Effect)
    , ("04320", SomeEffect theYithianRelicEffect)
    , ("04325", SomeEffect ichtacaScionOfYigEffect)
    , ("04326", SomeEffect alejandroVelaEffect)
    , ("05005", SomeEffect ritaYoungElderSignEffect)
    , ("05016", SomeEffect imDoneRunninEffect)
    , ("05018", SomeEffect mystifyingSongEffect)
    , ("05028", SomeEffect wellConnectedEffect)
    , ("05037", SomeEffect actOfDesperationEffect)
    , ("05040", SomeEffect aceOfRods1Effect)
    , ("05049", SomeEffect pennyWhiteEffect)
    , ("05085b", SomeEffect josefsPlanEffect)
    , ("05097", SomeEffect mysteriesOfTheLodgeEffect)
    , ("05114", SomeEffect meatCleaverEffect)
    , ("05157", SomeEffect witherEffect)
    , ("05158", SomeEffect sixthSenseEffect)
    , ("05171", SomeEffect hereticsGravesSpectral_171Effect)
    , ("05178j", SomeEffect unfinishedBusiness_JEffect)
    , ("05194", SomeEffect grislyTotemSeeker3Effect)
    , ("05195", SomeEffect grislyTotemSurvivor3Effect)
    , ("05201", SomeEffect infiltratingTheLodgeEffect)
    , ("05230", SomeEffect telescopicSight3Effect)
    , ("05239", SomeEffect theLoversVIEffect)
    , ("05251", SomeEffect unvisitedIsleStandingStonesEffect)
    , ("05254", SomeEffect unvisitedIsleMossCoveredStepsEffect)
    , ("05262", SomeEffect gavriellasFateEffect)
    , ("05263", SomeEffect jeromesFateEffect)
    , ("05264", SomeEffect pennysFateEffect)
    , ("05265", SomeEffect valentinosFateEffect)
    , ("05321", SomeEffect wither4Effect)
    , ("05322", SomeEffect sixthSense4Effect)
    , ("06056", SomeEffect enchantedWoodsLostWoodsEffect)
    , ("06082", SomeEffect corruptedOrderlyEffect)
    , ("06090", SomeEffect whispersOfHypnosEffect)
    , ("06114", SomeEffect followedEffect)
    , ("06115", SomeEffect momentum1Effect)
    , ("06162", SomeEffect gregoryGryEffect)
    , ("06195", SomeEffect thirtyFiveWinchesterEffect)
    , ("06201", SomeEffect spectralRazorEffect)
    , ("06278", SomeEffect surprisingFind1Effect)
    , ("06279", SomeEffect $ noop "06279")
    , ("06319", SomeEffect restlessJourneyFallacyEffect)
    , ("06320", SomeEffect restlessJourneyHardshipEffect)
    , ("06321", SomeEffect restlessJourneyLiesEffect)
    , ("06325", SomeEffect theEyeOfTruth5Effect)
    , ("07004", SomeEffect dexterDrakeEffect)
    , ("07012", SomeEffect showmanshipEffect)
    , ("07030", SomeEffect tidesOfFateEffect)
    , ("07042", SomeEffect awakeningEffect)
    , ("07043", SomeEffect theWaterRisesEffect)
    , ("07117", SomeEffect armageddonEffect)
    , ("07118", SomeEffect eyeOfChaosEffect)
    , ("07119", SomeEffect shroudOfShadowsEffect)
    , ("07161", SomeEffect thirdTimesACharm2Effect)
    , ("07210", SomeEffect longWayAroundEffect)
    , ("07222", SomeEffect fey1Effect)
    , ("07225", SomeEffect eyeOfTheDjinnVesselOfGoodAndEvil2Effect)
    , ("07226", SomeEffect armageddon4Effect)
    , ("07227", SomeEffect eyeOfChaos4Effect)
    , ("07228", SomeEffect shroudOfShadows4Effect)
    , ("07307", SomeEffect luckyDice3Effect)
    , ("08013a", SomeEffect disciplinePrescienceOfFateEffect)
    , ("08020", SomeEffect toeToToeEffect)
    , ("08021", SomeEffect getBehindMeEffect)
    , ("08034", SomeEffect writtenInTheStarsEffect)
    , ("08055", SomeEffect blackMarket2Effect)
    , ("08087", SomeEffect snipe1Effect)
    , ("08727", SomeEffect tekelili_227Effect)
    , ("09008", SomeEffect kymaniJonesEffect)
    , ("09009", SomeEffect grapplingHookEffect)
    , ("09029", SomeEffect oneInTheChamberEffect)
    , ("09041", SomeEffect empiricalHypothesisEffect)
    , ("09058", SomeEffect graysAnatomyTheDoctorsBible5Effect)
    , ("09066", SomeEffect hitAndRunEffect)
    , ("09087", SomeEffect explosiveWardEffect)
    , ("09109", SomeEffect atACrossroads1Effect)
    , ("09113", SomeEffect $ noop "09113")
    , ("10035", SomeEffect eyesOfValusiaTheMothersCunning4Effect)
    , ("10053", SomeEffect steadyHanded1Effect)
    , ("10056", SomeEffect prismaticSpectaclesLensToTheOtherworld2Effect)
    , ("10072", SomeEffect illPayYouBackEffect)
    , ("10102", SomeEffect spectralRazor2Effect)
    , ("10128", SomeEffect eldritchTongueEffect)
    , ("10129", SomeEffect bideYourTimeEffect)
    , ("10131", SomeEffect dawnStar1Effect)
    , ("50044", SomeEffect jeremiahPierceEffect)
    , ("52007", SomeEffect $ noop "52007")
    , ("52008", SomeEffect stormOfSpirits3Effect)
    , ("53007", SomeEffect mistsOfRlyeh2Effect)
    , ("54006", SomeEffect wellConnected3Effect)
    , ("60101", SomeEffect nathanielChoEffect)
    , ("60103", SomeEffect tommyMalloyEffect)
    , ("60220", SomeEffect $ noop "60220")
    , ("60232", SomeEffect miskatonicArchaeologyFunding4Effect)
    , ("60301", SomeEffect winifredHabbamockEffect)
    , ("60305", SomeEffect lockpicksEffect)
    , ("60323", SomeEffect cheapShot2Effect)
    , ("60324", SomeEffect slipAway2Effect)
    , ("60328", SomeEffect pilfer3Effect)
    , ("60329", SomeEffect backstab3Effect)
    , ("60330", SomeEffect copycat3Effect)
    , ("60402", SomeEffect arbiterOfFatesEffect)
    , ("60407", SomeEffect $ noop "60407")
    , ("60408", SomeEffect $ noop "60408")
    , ("60409", SomeEffect ineffableTruthEffect)
    , ("60411", SomeEffect crystalPendulumEffect)
    , ("60419", SomeEffect prescientEffect)
    , ("60425", SomeEffect $ noop "60425")
    , ("60426", SomeEffect $ noop "60426")
    , ("60427", SomeEffect ineffableTruth3Effect)
    , ("60430", SomeEffect $ noop "60430")
    , ("60431", SomeEffect $ noop "60431")
    , ("60432", SomeEffect ineffableTruth5Effect)
    , ("60512", SomeEffect willToSurviveEffect)
    , ("81007", SomeEffect cursedShoresEffect)
    , ("82026", SomeEffect gildedVoltoEffect)
    , ("82035", SomeEffect mesmerizeEffect)
    , ("84014", SomeEffect restaurantEffect)
    , ("84042", SomeEffect chillingPresenceEffect)
    , ("89004", SomeEffect realityAcid5U21Effect)
    , ("90002", SomeEffect daisysToteBagAdvancedEffect)
    , ("90009", SomeEffect onTheLamAdvancedEffect)
    , ("wmode", SomeEffect windowModifierEffect)
    , ("tokef", SomeEffect chaosTokenEffect)
    , ("ontok", SomeEffect onRevealChaosTokenEffect)
    , ("eotef", SomeEffect endOfTurnEffect)
    , ("surge", SomeEffect surgeEffect)
    , ("maxef", SomeEffect maxEffect)
    , ("noair", SomeEffect noAirEffect)
    ]
