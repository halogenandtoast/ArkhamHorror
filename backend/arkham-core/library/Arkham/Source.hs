{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Source where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Card
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.ChaosToken
import Arkham.Id
import Arkham.Matcher.Types (
  ActMatcher,
  AgendaMatcher,
  AssetMatcher,
  EnemyMatcher,
  LocationMatcher,
 )
import Arkham.Tarot
import Arkham.Trait
import Control.Lens (Prism', prism')
import Data.Aeson.TH
import GHC.OverloadedLabels
import GHC.Records

data Source
  = AbilitySource Source Int
  | ActiveCostSource ActiveCostId
  | ActDeckSource
  | ActSource ActId
  | AfterSkillTestSource
  | AgendaDeckSource
  | AgendaSource AgendaId
  | AgendaMatcherSource AgendaMatcher
  | AssetMatcherSource AssetMatcher
  | ActMatcherSource ActMatcher
  | AssetSource AssetId
  | CardCodeSource CardCode
  | CardSource Card
  | CardIdSource CardId
  | DeckSource
  | EffectSource EffectId
  | EmptyDeckSource
  | EncounterCardSource CardId
  | EnemyAttackSource EnemyId
  | EnemySource EnemyId
  | EventSource EventId
  | GameSource
  | InvestigatorSource InvestigatorId
  | LocationMatcherSource LocationMatcher
  | EnemyMatcherSource EnemyMatcher
  | LocationSource LocationId
  | PlayerCardSource PlayerCard
  | ProxySource {source :: Source, originalSource :: Source}
  | ResourceSource
  | ScenarioSource
  | SkillSource SkillId
  | SkillTestSource
  | StorySource StoryId
  | TestSource (Set Trait)
  | ChaosTokenSource ChaosToken
  | ChaosTokenEffectSource ChaosTokenFace
  | TreacherySource TreacheryId
  | YouSource
  | CampaignSource
  | ThisCard
  | CardCostSource CardId
  | BothSource Source Source
  | TarotSource TarotCard
  | BatchSource BatchId
  deriving stock (Show, Eq, Ord, Data)

_AssetSource :: Prism' Source AssetId
_AssetSource = prism' AssetSource $ \case
  AssetSource aid -> Just aid
  _ -> Nothing

instance HasField "asset" Source (Maybe AssetId) where
  getField = preview _AssetSource

$(deriveJSON defaultOptions ''Source)

instance ToJSONKey Source
instance FromJSONKey Source

class Sourceable a where
  toSource :: a -> Source
  isSource :: a -> Source -> Bool
  isSource = (==) . toSource

isProxySource :: Sourceable a => a -> Source -> Bool
isProxySource a (ProxySource _ source) = isSource a source
isProxySource _ _ = False

toProxySource :: Sourceable a => a -> Source -> Source
toProxySource a source = ProxySource source (toSource a)

proxy :: (Sourceable a, Sourceable b) => a -> b -> Source
proxy a b = ProxySource (toSource a) (toSource b)

instance Sourceable Source where
  toSource = id
  isSource = (==)

instance Sourceable a => Sourceable (a `With` b) where
  toSource (a `With` _) = toSource a
  isSource (a `With` _) = isSource a

instance Sourceable TreacheryId where
  toSource = TreacherySource

instance Sourceable InvestigatorId where
  toSource = InvestigatorSource

instance Sourceable LocationId where
  toSource = LocationSource

instance Sourceable AssetId where
  toSource = AssetSource

instance Sourceable AgendaId where
  toSource = AgendaSource

instance Sourceable EnemyId where
  toSource = EnemySource

instance Sourceable EventId where
  toSource = EventSource

instance Sourceable ChaosTokenFace where
  toSource = ChaosTokenEffectSource

instance Sourceable PlayerCard where
  toSource = PlayerCardSource

instance Sourceable AssetMatcher where
  toSource = AssetMatcherSource

instance Sourceable ActMatcher where
  toSource = ActMatcherSource

instance Sourceable LocationMatcher where
  toSource = LocationMatcherSource

toAbilitySource :: Sourceable a => a -> Int -> Source
toAbilitySource a n = case toSource a of
  AbilitySource b n' -> AbilitySource b n'
  b -> AbilitySource b n

isAbilitySource :: Sourceable a => a -> Int -> Source -> Bool
isAbilitySource a idx (AbilitySource b idx') | idx == idx' = isSource a b
isAbilitySource _ _ _ = False

isProxyAbilitySource :: Sourceable a => a -> Int -> Source -> Bool
isProxyAbilitySource a idx (AbilitySource (ProxySource _ b) idx') | idx == idx' = isSource a b
isProxyAbilitySource _ _ _ = False

pattern CultistEffect :: Source
pattern CultistEffect <- ChaosTokenEffectSource Arkham.ChaosToken.Cultist
  where
    CultistEffect = ChaosTokenEffectSource Arkham.ChaosToken.Cultist

pattern TabletEffect :: Source
pattern TabletEffect <- ChaosTokenEffectSource Tablet
  where
    TabletEffect = ChaosTokenEffectSource Tablet

pattern ElderThingEffect :: Source
pattern ElderThingEffect <- ChaosTokenEffectSource ElderThing
  where
    ElderThingEffect = ChaosTokenEffectSource ElderThing

instance IsLabel "elderSign" Source where
  fromLabel = ChaosTokenEffectSource ElderSign
