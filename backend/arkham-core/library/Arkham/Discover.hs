{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Discover where

import Arkham.Action
import Arkham.Id
import Arkham.Prelude
import Arkham.Source
import Data.Aeson.TH
import GHC.Records

data IsInvestigate = IsInvestigate | NotInvestigate
  deriving stock (Show, Eq, Generic, Data)

data DiscoverLocation = DiscoverYourLocation | DiscoverAtLocation LocationId
  deriving stock (Show, Eq, Data)

data Discover = Discover
  { discoverCount :: Int
  , discoverLocation :: DiscoverLocation
  , discoverSource :: Source
  , discoverAction :: Maybe Action
  }
  deriving stock (Show, Eq, Data)

instance HasField "count" Discover Int where
  getField = (.discoverCount)

instance HasField "location" Discover DiscoverLocation where
  getField = (.discoverLocation)

instance HasField "source" Discover Source where
  getField = (.discoverSource)

instance HasField "action" Discover (Maybe Action) where
  getField = (.discoverAction)

instance HasField "isInvestigate" Discover IsInvestigate where
  getField = discoverIsInvestigate

discoverIsInvestigate :: Discover -> IsInvestigate
discoverIsInvestigate d = if d.action == Just #investigate then IsInvestigate else NotInvestigate

viaInvestigate :: Discover -> Discover
viaInvestigate discover' = discover' {discoverAction = Just #investigate}

discoverAction :: Maybe Action -> Discover -> Discover
discoverAction action discover' = discover' {discoverAction = action}

discoverAtYourLocation :: Sourceable source => source -> Int -> Discover
discoverAtYourLocation (toSource -> source) n =
  Discover
    { discoverCount = n
    , discoverLocation = DiscoverYourLocation
    , discoverSource = source
    , discoverAction = Nothing
    }

discover
  :: (Sourceable source, AsId a, IdOf a ~ LocationId) => a -> source -> Int -> Discover
discover a (toSource -> source) n =
  Discover
    { discoverCount = n
    , discoverLocation = DiscoverAtLocation (asId a)
    , discoverSource = source
    , discoverAction = Nothing
    }

$( do
    isInvestigateJSON <- deriveJSON defaultOptions ''IsInvestigate
    discoverLocationJSON <- deriveJSON defaultOptions ''DiscoverLocation
    discoverJSON <- deriveJSON defaultOptions ''Discover
    pure $ concat [isInvestigateJSON, discoverLocationJSON, discoverJSON]
 )
