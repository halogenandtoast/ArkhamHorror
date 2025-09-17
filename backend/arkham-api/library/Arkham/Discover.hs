{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Discover where

import Arkham.Action
import Arkham.Id
import {-# SOURCE #-} Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Data.Aeson.TH
import GHC.Records

data IsInvestigate = IsInvestigate | NotInvestigate
  deriving stock (Show, Ord, Eq, Generic, Data)

data DiscoverLocation = DiscoverYourLocation | DiscoverAtLocation LocationId
  deriving stock (Show, Ord, Eq, Data)

data Discover = Discover
  { discoverCount :: Int
  , discoverLocation :: DiscoverLocation
  , discoverSource :: Source
  , discoverAction :: Maybe Action
  , discoverThen :: [Message]
  }
  deriving stock (Show, Ord, Eq, Data)

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

discoverAtYourLocation :: Sourceable source => source -> Int -> Discover
discoverAtYourLocation (toSource -> source) n =
  Discover
    { discoverCount = n
    , discoverLocation = DiscoverYourLocation
    , discoverSource = source
    , discoverAction = Nothing
    , discoverThen = []
    }

discover
  :: (Sourceable source, AsId a, IdOf a ~ LocationId) => a -> source -> Int -> Discover
discover a (toSource -> source) n =
  Discover
    { discoverCount = n
    , discoverLocation = DiscoverAtLocation (asId a)
    , discoverSource = source
    , discoverAction = Nothing
    , discoverThen = []
    }

mconcat
  [ deriveJSON defaultOptions ''IsInvestigate
  , deriveJSON defaultOptions ''DiscoverLocation
  , deriveJSON defaultOptions ''Discover
  ]
