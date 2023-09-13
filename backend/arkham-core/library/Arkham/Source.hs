{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Source where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Card
import Arkham.Card.CardCode
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.ChaosToken
import Arkham.Id
import Arkham.Matcher
import Arkham.Trait
import Data.Aeson.TH

data Source
  = AbilitySource Source Int
  | ActDeckSource
  | ActSource ActId
  | AfterSkillTestSource
  | AgendaDeckSource
  | AgendaSource AgendaId
  | AgendaMatcherSource AgendaMatcher
  | AssetMatcherSource AssetMatcher
  | AssetSource AssetId
  | CardCodeSource CardCode
  | CardSource Card
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
  deriving stock (Show, Eq, Ord)

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

instance Sourceable Source where
  toSource = id
  isSource = (==)

instance Sourceable a => Sourceable (a `With` b) where
  toSource (a `With` _) = toSource a
  isSource (a `With` _) = isSource a

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
