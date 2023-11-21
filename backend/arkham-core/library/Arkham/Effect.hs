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

import Arkham.Act.Acts (infiltratingTheLodgeEffect, theYithianRelicEffect)
import Arkham.Agenda.Agendas (lostMemoriesEffect, theLoversVIEffect, theRedDepthsEffect)
import Arkham.Asset.Assets (
  aceOfRods1Effect,
  alchemicalTransmutation2Effect,
  alchemicalTransmutationEffect,
  arbiterOfFatesEffect,
  azureFlame3Effect,
  azureFlame5Effect,
  azureFlameEffect,
  baseballBatEffect,
  clairvoyance3Effect,
  clairvoyance5Effect,
  clairvoyanceEffect,
  crystalPendulumEffect,
  fence1Effect,
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
  miskatonicArchaeologyFunding4Effect,
  mistsOfRlyeh2Effect,
  mistsOfRlyeh4Effect,
  mistsOfRlyehEffect,
  oldBookOfLore3Effect,
  pnakoticManuscripts5Effect,
  riteOfSeekingEffect,
  showmanshipEffect,
  shrivellingEffect,
  sixthSense4Effect,
  sixthSenseEffect,
  thirtyFiveWinchesterEffect,
  wellConnected3Effect,
  wellConnectedEffect,
  wither4Effect,
  witherEffect,
  yaotl1Effect,
 )
import Arkham.Enemy.Enemies (
  alejandroVelaEffect,
  boaConstrictorEffect,
  ichtacaScionOfYigEffect,
 )
import Arkham.Event.Events (
  aChanceEncounterEffect,
  actOfDesperationEffect,
  backstab3Effect,
  banish1Effect,
  blindingLight2Effect,
  blindingLightEffect,
  cheapShot2Effect,
  exposeWeakness3Effect,
  followedEffect,
  imDoneRunninEffect,
  marksmanship1Effect,
  mindOverMatter2Effect,
  mystifyingSongEffect,
  oneTwoPunch5Effect,
  oneTwoPunchEffect,
  pilfer3Effect,
  slipAwayEffect,
  spectralRazorEffect,
  stormOfSpirits3Effect,
  stormOfSpiritsEffect,
  telescopicSight3Effect,
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
  winifredHabbamockEffect,
 )
import Arkham.Location.Locations (
  hereticsGravesSpectral_171Effect,
  restaurantEffect,
  unvisitedIsleMossCoveredStepsEffect,
  unvisitedIsleStandingStonesEffect,
 )
import Arkham.Skill.Skills (
  copycat3Effect,
  defiance2Effect,
  defianceEffect,
  hatchetManEffect,
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
  arcaneBarrierEffect,
  chillingPresenceEffect,
  mysteriesOfTheLodgeEffect,
 )

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
    , ("01151", SomeEffect arkhamWoodsTwistingPaths)
    , ("02028", SomeEffect riteOfSeekingEffect)
    , ("02031", SomeEffect bindMonster2)
    , ("02100", SomeEffect pushedIntoTheBeyond)
    , ("02102", SomeEffect arcaneBarrierEffect)
    , ("02112", SomeEffect songOfTheDead2)
    , ("02114", SomeEffect fireExtinguisher1)
    , ("02150", SomeEffect deduction2)
    , ("02190", SomeEffect defianceEffect)
    , ("02228", SomeEffect exposeWeakness1)
    , ("02229", SomeEffect quickThinking)
    , ("02230", SomeEffect luckyDice2)
    , ("02236", SomeEffect undimensionedAndUnseenTabletToken)
    , ("02246", SomeEffect tenAcreMeadow_246)
    , ("02270", SomeEffect aChanceEncounterEffect)
    , ("02323", SomeEffect yogSothoth)
    , ("03005", SomeEffect williamYorick)
    , ("03012", SomeEffect thePaintedWorld)
    , ("03018", SomeEffect improvisation)
    , ("03022", SomeEffect letMeHandleThis)
    , ("03024", SomeEffect fieldwork)
    , ("03029", SomeEffect sleightOfHand)
    , ("03031", SomeEffect lockpicks1Effect)
    , ("03032", SomeEffect alchemicalTransmutationEffect)
    , ("03033", SomeEffect uncageTheSoulEffect)
    , ("03047a", SomeEffect theStrangerACityAflame)
    , ("03047b", SomeEffect theStrangerThePathIsMine)
    , ("03047c", SomeEffect theStrangerTheShoresOfHali)
    , ("03100", SomeEffect theKingsEdict)
    , ("03141", SomeEffect mrPeabody)
    , ("03149", SomeEffect charlesRossEsq)
    , ("03153", SomeEffect stormOfSpiritsEffect)
    , ("03155", SomeEffect fightOrFlight)
    , ("03158", SomeEffect callingInFavors)
    , ("03209", SomeEffect montmartre209)
    , ("03215", SomeEffect pereLachaiseCemetery)
    , ("03218", SomeEffect leMarais218)
    , ("03221a", SomeEffect theOrganistHopelessIDefiedHim)
    , ("03254", SomeEffect narrowShaft)
    , ("03306", SomeEffect eideticMemory3)
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
    , ("04232", SomeEffect slipAwayEffect)
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
    , ("05113", SomeEffect banish1Effect)
    , ("05114", SomeEffect meatCleaver)
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
    , ("06114", SomeEffect followedEffect)
    , ("06162", SomeEffect gregoryGryEffect)
    , ("06195", SomeEffect thirtyFiveWinchesterEffect)
    , ("06201", SomeEffect spectralRazorEffect)
    , ("06278", SomeEffect surprisingFind1Effect)
    , ("06279", SomeEffect oldBookOfLore3Effect)
    , ("06325", SomeEffect theEyeOfTruth5Effect)
    , ("07004", SomeEffect dexterDrakeEffect)
    , ("07012", SomeEffect showmanshipEffect)
    , ("50008", SomeEffect mindWipe3)
    , ("50044", SomeEffect jeremiahPierce)
    , ("52007", SomeEffect alchemicalTransmutation2Effect)
    , ("52008", SomeEffect stormOfSpirits3Effect)
    , ("53007", SomeEffect mistsOfRlyeh2Effect)
    , ("54006", SomeEffect wellConnected3Effect)
    , ("60101", SomeEffect nathanielChoEffect)
    , ("60103", SomeEffect tommyMalloy)
    , ("60117", SomeEffect oneTwoPunchEffect)
    , ("60132", SomeEffect oneTwoPunch5Effect)
    , ("60220", SomeEffect libraryDocent1Effect)
    , ("60226", SomeEffect mindOverMatter2Effect)
    , ("60232", SomeEffect miskatonicArchaeologyFunding4Effect)
    , ("60301", SomeEffect winifredHabbamockEffect)
    , ("60305", SomeEffect lockpicksEffect)
    , ("60323", SomeEffect cheapShot2Effect)
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
    , ("81001", SomeEffect curseOfTheRougarouTabletToken)
    , ("81007", SomeEffect cursedShores)
    , ("82026", SomeEffect gildedVolto)
    , ("82035", SomeEffect mesmerize)
    , ("84014", SomeEffect restaurantEffect)
    , ("84042", SomeEffect chillingPresenceEffect)
    , ("90002", SomeEffect daisysToteBagAdvanced)
    , ("wmode", SomeEffect windowModifierEffect)
    , ("tokef", SomeEffect chaosTokenEffect)
    , ("surge", SomeEffect surgeEffect)
    ]
