{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Arkham.Discover where

import Arkham.Action
import Arkham.Id
import {-# SOURCE #-} Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Data.Aeson.TH
import Data.UUID qualified as UUID
import GHC.Records

data IsInvestigate = IsInvestigate | NotInvestigate
  deriving stock (Show, Ord, Eq, Generic, Data)

data DiscoverLocation = DiscoverYourLocation | DiscoverAtLocation LocationId
  deriving stock (Show, Ord, Eq, Data)

data Discover = Discover
  { discoverId :: DiscoverId
  , discoverCount :: Int
  , discoverLocation :: DiscoverLocation
  , discoverSource :: Source
  , discoverAction :: Maybe Action
  , discoverThen :: [Message]
  }
  deriving stock (Show, Ord, Eq, Data)

instance HasField "id" Discover DiscoverId where
  getField = (.discoverId)

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

discoverAtYourLocation :: MonadRandom m => Sourceable source => source -> Int -> m Discover
discoverAtYourLocation (toSource -> source) n = do
  did <- getRandom
  pure
    $ Discover
      { discoverId = did
      , discoverCount = n
      , discoverLocation = DiscoverYourLocation
      , discoverSource = source
      , discoverAction = Nothing
      , discoverThen = []
      }

discover
  :: (Sourceable source, AsId a, IdOf a ~ LocationId, MonadRandom m) => a -> source -> Int -> m Discover
discover a (toSource -> source) n = do
  did <- getRandom
  pure $ discoverPure did a source n

discoverPure
  :: (Sourceable source, AsId a, IdOf a ~ LocationId) => DiscoverId -> a -> source -> Int -> Discover
discoverPure did a (toSource -> source) n = do
  Discover
    { discoverId = did
    , discoverCount = n
    , discoverLocation = DiscoverAtLocation (asId a)
    , discoverSource = source
    , discoverAction = Nothing
    , discoverThen = []
    }

mconcat
  [ deriveJSON defaultOptions ''IsInvestigate
  , deriveJSON defaultOptions ''DiscoverLocation
  , deriveToJSON defaultOptions ''Discover
  ]

instance FromJSON Discover where
  parseJSON = withObject "Discover" \o -> do
    discoverId <- o .:? "discoverId" .!= DiscoverId UUID.nil
    discoverCount <- o .: "discoverCount"
    discoverLocation <- o .: "discoverLocation"
    discoverSource <- o .: "discoverSource"
    discoverAction <- o .:? "discoverAction"
    discoverThen <- o .:? "discoverThen" .!= []
    pure Discover {..}
