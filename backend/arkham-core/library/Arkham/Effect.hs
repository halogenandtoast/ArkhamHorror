{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Effect (
  module Arkham.Effect,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Effect.Effects
import Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Id
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
import Arkham.Agenda.Agendas (lostMemoriesEffect, theLoversVIEffect, theRedDepthsEffect)
import Arkham.Asset.Assets (
  aceOfRods1Effect,
  alchemicalTransmutation2Effect,
  alchemicalTransmutationEffect,
  arbiterOfFatesEffect,
  armageddon4Effect,
  armageddonEffect,
  azureFlame3Effect,
  azureFlame5Effect,
  azureFlameEffect,
  baseballBatEffect,
  charlesRossEsqEffect,
  clairvoyance3Effect,
  clairvoyance5Effect,
  clairvoyanceEffect,
  crystalPendulumEffect,
  daisysToteBagAdvancedEffect,
  empiricalHypothesisEffect,
  eyeOfChaos4Effect,
  eyeOfChaosEffect,
  fence1Effect,
  fieldworkEffect,
  fireExtinguisher1Effect,
  gildedVoltoEffect,
  gregoryGryEffect,
  grislyTotemSeeker3Effect,
  grislyTotemSurvivor3Effect,
  highRoller2Effect,
  ineffableTruth3Effect,
  ineffableTruth5Effect,
  ineffableTruthEffect,
  libraryDocent1Effect,
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
  oldBookOfLore3Effect,
  pnakoticManuscripts5Effect,
  riteOfSeekingEffect,
  showmanshipEffect,
  shrivellingEffect,
  shroudOfShadows4Effect,
  shroudOfShadowsEffect,
  sixthSense4Effect,
  sixthSenseEffect,
  songOfTheDead2Effect,
  thirtyFiveWinchesterEffect,
  twilightBladeEffect,
  wellConnected3Effect,
  wellConnectedEffect,
  wither4Effect,
  witherEffect,
  yaotl1Effect,
 )
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
  backstab3Effect,
  bindMonster2Effect,
  blindingLight2Effect,
  blindingLightEffect,
  callingInFavorsEffect,
  cheapShot2Effect,
  eideticMemory3Effect,
  exposeWeakness1Effect,
  exposeWeakness3Effect,
  fightOrFlightEffect,
  followedEffect,
  imDoneRunninEffect,
  improvisationEffect,
  letMeHandleThisEffect,
  marksmanship1Effect,
  mystifyingSongEffect,
  pilfer3Effect,
  sleightOfHandEffect,
  slipAway2Effect,
  spectralRazorEffect,
  stormOfSpirits3Effect,
  stormOfSpiritsEffect,
  telescopicSight3Effect,
  thePaintedWorldEffect,
  thirdTimesACharm2Effect,
  tidesOfFateEffect,
  uncageTheSoulEffect,
  vantagePointEffect,
  willToSurviveEffect,
 )
import Arkham.Investigator.Investigators (
  dexterDrakeEffect,
  fatherMateoElderSignEffect,
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
  montmartre209Effect,
  narrowShaftEffect,
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
  quickThinkingEffect,
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
  restlessJourneyFallacyEffect,
  restlessJourneyHardshipEffect,
  restlessJourneyLiesEffect,
  theKingsEdictEffect,
  whispersOfHypnosEffect,
 )

noop :: CardCode -> EffectArgs -> NoEffect
noop cCode = NoEffect . uncurry4 (baseAttrs cCode)

newtype NoEffect = NoEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance RunMessage NoEffect where
  runMessage msg (NoEffect a) = NoEffect <$> runMessage msg a

createEffect
  :: MonadRandom m
  => CardCode
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> m (EffectId, Effect)
createEffect cardCode meffectMetadata source target = do
  eid <- getRandom
  pure (eid, lookupEffect cardCode eid meffectMetadata source target)

createChaosTokenValueEffect
  :: MonadRandom m => Int -> Source -> Target -> m (EffectId, Effect)
createChaosTokenValueEffect n source target = do
  eid <- getRandom
  pure (eid, buildChaosTokenValueEffect eid n source target)

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

createSurgeEffect
  :: (MonadRandom m, Sourceable source, Targetable target)
  => source
  -> target
  -> m (EffectId, Effect)
createSurgeEffect (toSource -> source) (toTarget -> target) = do
  eid <- getRandom
  pure
    ( eid
    , Effect $ surgeEffect (eid, Nothing, source, target)
    )

instance RunMessage Effect where
  runMessage msg (Effect a) = Effect <$> runMessage msg a

lookupEffect
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> Effect
lookupEffect cardCode eid mmetadata source target =
  case lookup cardCode allEffects of
    Nothing -> error $ "Unknown effect: " <> show cardCode
    Just (SomeEffect f) -> Effect $ f (eid, mmetadata, source, target)

buildChaosTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildChaosTokenValueEffect eid n source =
  buildWindowModifierEffect
    eid
    (EffectModifiers [Modifier source (ChaosTokenValueModifier n) False])
    EffectSkillTestWindow
    source

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
    [ ("01060", SomeEffect shrivellingEffect)
    , ("01066", SomeEffect blindingLightEffect)
    , ("01069", SomeEffect blindingLight2Effect)
    , ("01074", SomeEffect baseballBatEffect)
    , ("02028", SomeEffect riteOfSeekingEffect)
    , ("02031", SomeEffect bindMonster2Effect)
    , ("02100", SomeEffect pushedIntoTheBeyondEffect)
    , ("02112", SomeEffect songOfTheDead2Effect)
    , ("02114", SomeEffect fireExtinguisher1Effect)
    , ("02150", SomeEffect deduction2Effect)
    , ("02190", SomeEffect defianceEffect)
    , ("02228", SomeEffect exposeWeakness1Effect)
    , ("02229", SomeEffect quickThinkingEffect)
    , ("02230", SomeEffect luckyDice2Effect)
    , ("02236", SomeEffect undimensionedAndUnseenTabletToken)
    , ("02246", SomeEffect tenAcreMeadow_246Effect)
    , ("02270", SomeEffect aChanceEncounterEffect)
    , ("02323", SomeEffect yogSothothEffect)
    , ("03005", SomeEffect williamYorickEffect)
    , ("03012", SomeEffect thePaintedWorldEffect)
    , ("03018", SomeEffect improvisationEffect)
    , ("03022", SomeEffect letMeHandleThisEffect)
    , ("03024", SomeEffect fieldworkEffect)
    , ("03029", SomeEffect sleightOfHandEffect)
    , ("03031", SomeEffect lockpicks1Effect)
    , ("03032", SomeEffect alchemicalTransmutationEffect)
    , ("03033", SomeEffect uncageTheSoulEffect)
    , ("03047a", SomeEffect theStrangerACityAflameEffect)
    , ("03047b", SomeEffect theStrangerThePathIsMineEffect)
    , ("03047c", SomeEffect theStrangerTheShoresOfHaliEffect)
    , ("03100", SomeEffect theKingsEdictEffect)
    , ("03141", SomeEffect mrPeabodyEffect)
    , ("03149", SomeEffect charlesRossEsqEffect)
    , ("03153", SomeEffect stormOfSpiritsEffect)
    , ("03155", SomeEffect fightOrFlightEffect)
    , ("03158", SomeEffect callingInFavorsEffect)
    , ("03209", SomeEffect montmartre209Effect)
    , ("03254", SomeEffect narrowShaftEffect)
    , ("03306", SomeEffect eideticMemory3Effect)
    , ("04004", SomeEffect fatherMateoElderSignEffect)
    , ("04029", SomeEffect mistsOfRlyehEffect)
    , ("04035", SomeEffect yaotl1Effect)
    , ("04079", SomeEffect boaConstrictorEffect)
    , ("04104", SomeEffect marksmanship1Effect)
    , ("04108", SomeEffect fence1Effect)
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
    , ("05255", SomeEffect unvisitedIsleMossCoveredStepsEffect)
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
    , ("06279", SomeEffect oldBookOfLore3Effect)
    , ("06319", SomeEffect restlessJourneyFallacyEffect)
    , ("06320", SomeEffect restlessJourneyHardshipEffect)
    , ("06321", SomeEffect restlessJourneyLiesEffect)
    , ("06325", SomeEffect theEyeOfTruth5Effect)
    , ("07004", SomeEffect dexterDrakeEffect)
    , ("07012", SomeEffect showmanshipEffect)
    , ("07030", SomeEffect tidesOfFateEffect)
    , ("07117", SomeEffect armageddonEffect)
    , ("07118", SomeEffect eyeOfChaosEffect)
    , ("07119", SomeEffect shroudOfShadowsEffect)
    , ("07161", SomeEffect thirdTimesACharm2Effect)
    , ("07222", SomeEffect fey1Effect)
    , ("07226", SomeEffect armageddon4Effect)
    , ("07227", SomeEffect eyeOfChaos4Effect)
    , ("07228", SomeEffect shroudOfShadows4Effect)
    , ("07307", SomeEffect luckyDice3Effect)
    , ("09041", SomeEffect empiricalHypothesisEffect)
    , ("50013", SomeEffect twilightBladeEffect)
    , ("50044", SomeEffect jeremiahPierceEffect)
    , ("52007", SomeEffect alchemicalTransmutation2Effect)
    , ("52008", SomeEffect stormOfSpirits3Effect)
    , ("53007", SomeEffect mistsOfRlyeh2Effect)
    , ("54006", SomeEffect wellConnected3Effect)
    , ("60101", SomeEffect nathanielChoEffect)
    , ("60103", SomeEffect tommyMalloyEffect)
    , ("60220", SomeEffect libraryDocent1Effect)
    , ("60232", SomeEffect miskatonicArchaeologyFunding4Effect)
    , ("60301", SomeEffect winifredHabbamockEffect)
    , ("60305", SomeEffect lockpicksEffect)
    , ("60323", SomeEffect cheapShot2Effect)
    , ("60324", SomeEffect slipAway2Effect)
    , ("60328", SomeEffect pilfer3Effect)
    , ("60329", SomeEffect backstab3Effect)
    , ("60330", SomeEffect copycat3Effect)
    , ("60402", SomeEffect arbiterOfFatesEffect)
    , ("60407", SomeEffect azureFlameEffect)
    , ("60408", SomeEffect clairvoyanceEffect)
    , ("60409", SomeEffect ineffableTruthEffect)
    , ("60411", SomeEffect crystalPendulumEffect)
    , ("60419", SomeEffect prescientEffect)
    , ("60425", SomeEffect azureFlame3Effect)
    , ("60426", SomeEffect clairvoyance3Effect)
    , ("60427", SomeEffect ineffableTruth3Effect)
    , ("60430", SomeEffect azureFlame5Effect)
    , ("60431", SomeEffect clairvoyance5Effect)
    , ("60432", SomeEffect ineffableTruth5Effect)
    , ("60512", SomeEffect willToSurviveEffect)
    , ("81007", SomeEffect cursedShoresEffect)
    , ("82026", SomeEffect gildedVoltoEffect)
    , ("82035", SomeEffect mesmerizeEffect)
    , ("84014", SomeEffect restaurantEffect)
    , ("84042", SomeEffect chillingPresenceEffect)
    , ("90002", SomeEffect daisysToteBagAdvancedEffect)
    , ("wmode", SomeEffect windowModifierEffect)
    , ("tokef", SomeEffect chaosTokenEffect)
    , ("surge", SomeEffect surgeEffect)
    ]
