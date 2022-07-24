module Arkham.Effect
  ( module Arkham.Effect
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Effect.Attrs
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
import Data.Typeable

import Arkham.Asset.Assets
  ( MistsOfRlyehEffect (..)
  , Yaotl1Effect (..)
  , mistsOfRlyehEffect
  , yaotl1Effect
  )
import Arkham.Event.Events
  ( OneTwoPunch5Effect (..)
  , OneTwoPunchEffect (..)
  , oneTwoPunch5Effect
  , oneTwoPunchEffect
  , WillToSurviveEffect(..)
  , willToSurviveEffect
  )
-- start importing directly
import Arkham.Investigator.Investigators
  ( FatherMateoElderSignEffect (..), fatherMateoElderSignEffect )

data Effect = forall a . IsEffect a => Effect a

instance Eq Effect where
  (Effect (a :: a)) == (Effect (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance Show Effect where
  show (Effect a) = show a

instance ToJSON Effect where
  toJSON (Effect a) = toJSON a

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

instance HasModifiersFor Effect where
  getModifiersFor source target (Effect a) = getModifiersFor source target a

instance HasAbilities Effect where
  getAbilities (Effect a) = getAbilities a

instance RunMessage Effect where
  runMessage msg (Effect a) = Effect <$> runMessage msg a

instance Entity Effect where
  type EntityId Effect = EffectId
  type EntityAttrs Effect = EffectAttrs
  toId = toId . toAttrs
  toAttrs (Effect a) = toAttrs a
  overAttrs f (Effect a) = Effect $ overAttrs f a

instance TargetEntity Effect where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Effect where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupEffect
  :: CardCode
  -> EffectId
  -> Maybe (EffectMetadata Window Message)
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
  [ ("01010", Effect . onTheLam)
  , ("01036", Effect . mindOverMatter)
  , ("01060", Effect . shrivelling)
  , ("01066", Effect . blindingLight)
  , ("01068", Effect . mindWipe1)
  , ("01069", Effect . blindingLight2)
  , ("01074", Effect . baseballBat)
  , ("01080", Effect . lucky)
  , ("01084", Effect . lucky2)
  , ("01085", Effect . willToSurvive3)
  , ("01088", Effect . sureGamble3)
  , ("01151", Effect . arkhamWoodsTwistingPaths)
  , ("02028", Effect . riteOfSeeking)
  , ("02031", Effect . bindMonster2)
  , ("02100", Effect . pushedIntoTheBeyond)
  , ("02102", Effect . arcaneBarrier)
  , ("02112", Effect . songOfTheDead2)
  , ("02114", Effect . fireExtinguisher1)
  , ("02150", Effect . deduction2)
  , ("02228", Effect . exposeWeakness1)
  , ("02229", Effect . quickThinking)
  , ("02230", Effect . luckyDice2)
  , ("02233", Effect . riteOfSeeking4)
  , ("02236", Effect . undimensionedAndUnseenTabletToken)
  , ("02246", Effect . tenAcreMeadow_246)
  , ("02270", Effect . aChanceEncounter)
  , ("02323", Effect . yogSothoth)
  , ("03002", Effect . minhThiPhan)
  , ("03005", Effect . williamYorick)
  , ("03012", Effect . thePaintedWorld)
  , ("03018", Effect . improvisation)
  , ("03022", Effect . letMeHandleThis)
  , ("03024", Effect . fieldwork)
  , ("03029", Effect . sleightOfHand)
  , ("03031", Effect . lockpicks1)
  , ("03032", Effect . alchemicalTransmutation)
  , ("03033", Effect . uncageTheSoul)
  , ("03040", Effect . overzealous)
  , ("03047a", Effect . theStrangerACityAflame)
  , ("03047b", Effect . theStrangerThePathIsMine)
  , ("03047c", Effect . theStrangerTheShoresOfHali)
  , ("03100", Effect . theKingsEdict)
  , ("03141", Effect . mrPeabody)
  , ("03149", Effect . charlesRossEsq)
  , ("03153", Effect . stormOfSpirits)
  , ("03155", Effect . fightOrFlight)
  , ("03158", Effect . callingInFavors)
  , ("03209", Effect . montmartre209)
  , ("03215", Effect . pereLachaiseCemetery)
  , ("03218", Effect . leMarais218)
  , ("03221a", Effect . theOrganistHopelessIDefiedHim)
  , ("03254", Effect . narrowShaft)
  , ("03306", Effect . eideticMemory3)
  , ("04004", Effect . fatherMateoElderSignEffect)
  , ("04029", Effect . mistsOfRlyehEffect)
  , ("04035", Effect . yaotl1Effect)
  , ("05114", Effect . meatCleaver)
  , ("50008", Effect . mindWipe3)
  , ("50033", Effect . arkhamWoodsGreatWillow)
  , ("50044", Effect . jeremiahPierce)
  , ("60101", Effect . nathanielCho)
  , ("60103", Effect . tommyMalloy)
  , ("60117", Effect . oneTwoPunchEffect)
  , ("60132", Effect . oneTwoPunch5Effect)
  , ("60305", Effect . lockpicks)
  , ("60505", Effect . eighteenDerringer)
  , ("81001", Effect . curseOfTheRougarouTabletToken)
  , ("81007", Effect . cursedShores)
  , ("82026", Effect . gildedVolto)
  , ("82035", Effect . mesmerize)
  , ("90002", Effect . daisysToteBagAdvanced)
  ]

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
  Effect $ windowModifierEffect eid metadata effectWindow source target

buildTokenEffect
  :: EffectId -> EffectMetadata Window Message -> Source -> Token -> Effect
buildTokenEffect eid metadata source token =
  Effect $ tokenEffect eid metadata source token

instance FromJSON Effect where
  parseJSON v = flip (withObject "Effect") v $ \o -> do
    cCode :: CardCode <- o .: "cardCode"
    case cCode of
      "01010" -> Effect . OnTheLam <$> parseJSON v
      "01036" -> Effect . MindOverMatter <$> parseJSON v
      "01060" -> Effect . Shrivelling <$> parseJSON v
      "01066" -> Effect . BlindingLight <$> parseJSON v
      "01068" -> Effect . MindWipe1 <$> parseJSON v
      "01069" -> Effect . BlindingLight2 <$> parseJSON v
      "01074" -> Effect . BaseballBat <$> parseJSON v
      "01080" -> Effect . Lucky <$> parseJSON v
      "01084" -> Effect . Lucky2 <$> parseJSON v
      "01085" -> Effect . WillToSurvive3 <$> parseJSON v
      "01088" -> Effect . SureGamble3 <$> parseJSON v
      "01151" -> Effect . ArkhamWoodsTwistingPaths <$> parseJSON v
      "02028" -> Effect . RiteOfSeeking <$> parseJSON v
      "02031" -> Effect . BindMonster2 <$> parseJSON v
      "02100" -> Effect . PushedIntoTheBeyond <$> parseJSON v
      "02102" -> Effect . ArcaneBarrier <$> parseJSON v
      "02112" -> Effect . SongOfTheDead2 <$> parseJSON v
      "02114" -> Effect . FireExtinguisher1 <$> parseJSON v
      "02150" -> Effect . Deduction2 <$> parseJSON v
      "02228" -> Effect . ExposeWeakness1 <$> parseJSON v
      "02229" -> Effect . QuickThinking <$> parseJSON v
      "02230" -> Effect . LuckyDice2 <$> parseJSON v
      "02233" -> Effect . RiteOfSeeking4 <$> parseJSON v
      "02236" -> Effect . UndimensionedAndUnseenTabletToken <$> parseJSON v
      "02246" -> Effect . TenAcreMeadow_246 <$> parseJSON v
      "02270" -> Effect . AChanceEncounter <$> parseJSON v
      "02323" -> Effect . YogSothoth <$> parseJSON v
      "03002" -> Effect . MinhThiPhan <$> parseJSON v
      "03005" -> Effect . WilliamYorick <$> parseJSON v
      "03012" -> Effect . ThePaintedWorld <$> parseJSON v
      "03018" -> Effect . Improvisation <$> parseJSON v
      "03022" -> Effect . LetMeHandleThis <$> parseJSON v
      "03024" -> Effect . Fieldwork <$> parseJSON v
      "03029" -> Effect . SleightOfHand <$> parseJSON v
      "03031" -> Effect . Lockpicks1 <$> parseJSON v
      "03032" -> Effect . AlchemicalTransmutation <$> parseJSON v
      "03033" -> Effect . UncageTheSoul <$> parseJSON v
      "03040" -> Effect . Overzealous <$> parseJSON v
      "03047a" -> Effect . TheStrangerACityAflame <$> parseJSON v
      "03047b" -> Effect . TheStrangerThePathIsMine <$> parseJSON v
      "03047c" -> Effect . TheStrangerTheShoresOfHali <$> parseJSON v
      "03100" -> Effect . TheKingsEdict <$> parseJSON v
      "03141" -> Effect . MrPeabody <$> parseJSON v
      "03149" -> Effect . CharlesRossEsq <$> parseJSON v
      "03153" -> Effect . StormOfSpirits <$> parseJSON v
      "03155" -> Effect . FightOrFlight <$> parseJSON v
      "03158" -> Effect . CallingInFavors <$> parseJSON v
      "03209" -> Effect . Montmartre209 <$> parseJSON v
      "03215" -> Effect . PereLachaiseCemetery <$> parseJSON v
      "03218" -> Effect . LeMarais218 <$> parseJSON v
      "03221a" -> Effect . TheOrganistHopelessIDefiedHim <$> parseJSON v
      "03254" -> Effect . NarrowShaft <$> parseJSON v
      "03306" -> Effect . EideticMemory3 <$> parseJSON v
      "04004" -> Effect . FatherMateoElderSignEffect <$> parseJSON v
      "04029" -> Effect . MistsOfRlyehEffect <$> parseJSON v
      "04035" -> Effect . Yaotl1Effect <$> parseJSON v
      "05114" -> Effect . MeatCleaver <$> parseJSON v
      "50008" -> Effect . MindWipe3 <$> parseJSON v
      "50033" -> Effect . ArkhamWoodsGreatWillow <$> parseJSON v
      "50044" -> Effect . JeremiahPierce <$> parseJSON v
      "60101" -> Effect . NathanielCho <$> parseJSON v
      "60103" -> Effect . TommyMalloy <$> parseJSON v
      "60117" -> Effect . OneTwoPunchEffect <$> parseJSON v
      "60132" -> Effect . OneTwoPunch5Effect <$> parseJSON v
      "60305" -> Effect . Lockpicks <$> parseJSON v
      "60505" -> Effect . EighteenDerringer <$> parseJSON v
      "81001" -> Effect . CurseOfTheRougarouTabletToken <$> parseJSON v
      "81007" -> Effect . CursedShores <$> parseJSON v
      "82026" -> Effect . GildedVolto <$> parseJSON v
      "82035" -> Effect . Mesmerize <$> parseJSON v
      "90002" -> Effect . DaisysToteBagAdvanced <$> parseJSON v
      "wmode" -> Effect . WindowModifierEffect <$> parseJSON v
      "tokef" -> Effect . TokenEffect <$> parseJSON v
      _ -> error "invalid effect"
