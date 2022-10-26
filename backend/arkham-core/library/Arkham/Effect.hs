{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Effect
  ( module Arkham.Effect
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Types
import Arkham.Effect.Effects
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Id
import Arkham.Message
import Arkham.Modifier
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Window ( Window )

-- start importing directly
import Arkham.Asset.Assets
  ( mistsOfRlyehEffect
  , yaotl1Effect
  , fence1Effect
  , highRoller2Effect
  )
import Arkham.Enemy.Enemies
  ( boaConstrictorEffect
  )
import Arkham.Event.Events
  ( oneTwoPunch5Effect
  , oneTwoPunchEffect
  , willToSurviveEffect
  , marksmanship1Effect
  , exposeWeakness3Effect
  , slipAwayEffect
  )
import Arkham.Skill.Skills
  ( hatchetManEffect
  )
import Arkham.Investigator.Investigators
  ( fatherMateoElderSignEffect )

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

createTokenValueEffect
  :: MonadRandom m => Int -> Source -> Target -> m (EffectId, Effect)
createTokenValueEffect n source target = do
  eid <- getRandom
  pure (eid, buildTokenValueEffect eid n source target)

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

createTokenEffect
  :: MonadRandom m
  => EffectMetadata Window Message
  -> Source
  -> Token
  -> m (EffectId, Effect)
createTokenEffect effectMetadata source token = do
  eid <- getRandom
  pure (eid, buildTokenEffect eid effectMetadata source token)

instance RunMessage Effect where
  runMessage msg (Effect a) = Effect <$> runMessage msg a

lookupEffect
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Window Message)
  -> Source
  -> Target
  -> Effect
lookupEffect cardCode eid mmetadata source target = case lookup cardCode allEffects of
  Nothing -> error $ "Unknown effect: " <> show cardCode
  Just (SomeEffect f) -> Effect $ f (eid, mmetadata, source, target)

buildTokenValueEffect :: EffectId -> Int -> Source -> Target -> Effect
buildTokenValueEffect eid n source = buildWindowModifierEffect
  eid
  (EffectModifiers [Modifier source $ TokenValueModifier n])
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

buildTokenEffect
  :: EffectId -> EffectMetadata Window Message -> Source -> Token -> Effect
buildTokenEffect eid metadata source token =
  Effect $ tokenEffect' eid metadata source token

instance FromJSON Effect where
  parseJSON v = flip (withObject "Effect") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case lookup cCode allEffects of
      Nothing -> error $ "Invalid effect: " <> show cCode
      Just (SomeEffect (_ :: EffectArgs ->a)) -> Effect <$> parseJSON @a v

allEffects :: HashMap CardCode SomeEffect
allEffects = mapFromList
  [ ("01010", SomeEffect onTheLam)
  , ("01036", SomeEffect mindOverMatter)
  , ("01060", SomeEffect shrivelling)
  , ("01066", SomeEffect blindingLight)
  , ("01068", SomeEffect mindWipe1)
  , ("01069", SomeEffect blindingLight2)
  , ("01074", SomeEffect baseballBat)
  , ("01085", SomeEffect willToSurvive3)
  , ("01088", SomeEffect sureGamble3)
  , ("01151", SomeEffect arkhamWoodsTwistingPaths)
  , ("02028", SomeEffect riteOfSeeking)
  , ("02031", SomeEffect bindMonster2)
  , ("02100", SomeEffect pushedIntoTheBeyond)
  , ("02102", SomeEffect arcaneBarrier)
  , ("02112", SomeEffect songOfTheDead2)
  , ("02114", SomeEffect fireExtinguisher1)
  , ("02150", SomeEffect deduction2)
  , ("02228", SomeEffect exposeWeakness1)
  , ("02229", SomeEffect quickThinking)
  , ("02230", SomeEffect luckyDice2)
  , ("02233", SomeEffect riteOfSeeking4)
  , ("02236", SomeEffect undimensionedAndUnseenTabletToken)
  , ("02246", SomeEffect tenAcreMeadow_246)
  , ("02270", SomeEffect aChanceEncounter)
  , ("02323", SomeEffect yogSothoth)
  , ("03002", SomeEffect minhThiPhan)
  , ("03005", SomeEffect williamYorick)
  , ("03012", SomeEffect thePaintedWorld)
  , ("03018", SomeEffect improvisation)
  , ("03022", SomeEffect letMeHandleThis)
  , ("03024", SomeEffect fieldwork)
  , ("03029", SomeEffect sleightOfHand)
  , ("03031", SomeEffect lockpicks1)
  , ("03032", SomeEffect alchemicalTransmutation)
  , ("03033", SomeEffect uncageTheSoul)
  , ("03040", SomeEffect overzealous)
  , ("03047a", SomeEffect theStrangerACityAflame)
  , ("03047b", SomeEffect theStrangerThePathIsMine)
  , ("03047c", SomeEffect theStrangerTheShoresOfHali)
  , ("03100", SomeEffect theKingsEdict)
  , ("03141", SomeEffect mrPeabody)
  , ("03149", SomeEffect charlesRossEsq)
  , ("03153", SomeEffect stormOfSpirits)
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
  , ("04232", SomeEffect slipAwayEffect)
  , ("05114", SomeEffect meatCleaver)
  , ("50008", SomeEffect mindWipe3)
  , ("50033", SomeEffect arkhamWoodsGreatWillow)
  , ("50044", SomeEffect jeremiahPierce)
  , ("60101", SomeEffect nathanielCho)
  , ("60103", SomeEffect tommyMalloy)
  , ("60117", SomeEffect oneTwoPunchEffect)
  , ("60132", SomeEffect oneTwoPunch5Effect)
  , ("60305", SomeEffect lockpicks)
  , ("60512", SomeEffect willToSurviveEffect)
  , ("81001", SomeEffect curseOfTheRougarouTabletToken)
  , ("81007", SomeEffect cursedShores)
  , ("82026", SomeEffect gildedVolto)
  , ("82035", SomeEffect mesmerize)
  , ("90002", SomeEffect daisysToteBagAdvanced)
  , ("wmode", SomeEffect windowModifierEffect)
  , ("tokef", SomeEffect tokenEffect)
  ]
