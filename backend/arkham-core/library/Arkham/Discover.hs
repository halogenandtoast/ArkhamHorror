module Arkham.Discover where

import Arkham.Prelude

import Arkham.Action
import Arkham.Id
import Arkham.Source
import GHC.Records

data DiscoverLocation = DiscoverYourLocation | DiscoverAtLocation LocationId

data Discover = Discover
  { discoverCount :: Int
  , discoverInvestigator :: InvestigatorId
  , discoverLocation :: DiscoverLocation
  , discoverSource :: Source
  , discoverAction :: Maybe Action
  }

instance HasField "count" Discover Int where
  getField = discoverCount

instance HasField "investigator" Discover InvestigatorId where
  getField = discoverInvestigator

instance HasField "location" Discover DiscoverLocation where
  getField = discoverLocation

instance HasField "source" Discover Source where
  getField = discoverSource

instance HasField "action" Discover (Maybe Action) where
  getField = discoverAction

viaInvestigate :: Discover -> Discover
viaInvestigate discover' = discover' {discoverAction = Just #investigate}

discoverAtYourLocation :: Sourceable source => InvestigatorId -> source -> Int -> Discover
discoverAtYourLocation iid (toSource -> source) n =
  Discover
    { discoverCount = n
    , discoverInvestigator = iid
    , discoverLocation = DiscoverYourLocation
    , discoverSource = source
    , discoverAction = Nothing
    }

discover
  :: (Sourceable source, AsId a, IdOf a ~ LocationId) => InvestigatorId -> a -> source -> Int -> Discover
discover iid a (toSource -> source) n =
  Discover
    { discoverCount = n
    , discoverInvestigator = iid
    , discoverLocation = DiscoverAtLocation (asId a)
    , discoverSource = source
    , discoverAction = Nothing
    }
