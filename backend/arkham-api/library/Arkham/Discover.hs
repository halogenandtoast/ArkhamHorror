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

{- | Which of the three relationships a clue discovery can have to an
investigation. Two separate questions used to share one flag, and Trish
Scarborough needs opposite answers to them (#6043), so they are split:
'countsAsInvestigate' is the permission half and
'takesInvestigateClueBonus' is the bonus half.
-}
data IsInvestigate
  = {- | The investigate action's own discovery. Counts as a result of
    investigating, and takes the test's "discover 1 additional clue" bonuses.
    -}
    IsInvestigate
  | {- | Its own discovery, caused by an investigation (Trish Scarborough's
    reaction). Counts as a result of investigating, so a location that only
    allows discovery by investigating still allows it -- but the investigation's
    clue bonus has already been paid out to the discovery above and must not be
    paid again here.
    -}
    AsResultOfInvestigate
  | NotInvestigate
  deriving stock (Show, Ord, Eq, Generic, Data)

{- | The permission half: whether this discovery survives a
'CannotDiscoverCluesExceptAsResultOfInvestigation' (Shrine to Hydra, The Summit,
Schoolhouse, The Black Core, Shadow Agents).
-}
countsAsInvestigate :: IsInvestigate -> Bool
countsAsInvestigate = (/= NotInvestigate)

{- | The bonus half: whether this discovery takes the 'DiscoveredClues' /
'DiscoveredCluesAt' modifiers off the investigator. Only the investigate
action's own discovery does; anything else would pay the bonus twice.
-}
takesInvestigateClueBonus :: IsInvestigate -> Bool
takesInvestigateClueBonus = (== IsInvestigate)

data DiscoverLocation = DiscoverYourLocation | DiscoverAtLocation LocationId
  deriving stock (Show, Ord, Eq, Data)

data Discover = Discover
  { discoverId :: DiscoverId
  , discoverCount :: Int
  , discoverLocation :: DiscoverLocation
  , discoverSource :: Source
  , discoverVia :: IsInvestigate
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
  getField d = guard (countsAsInvestigate d.discoverVia) $> #investigate

instance HasField "isInvestigate" Discover IsInvestigate where
  getField = (.discoverVia)

viaInvestigate :: Discover -> Discover
viaInvestigate discover' = discover' {discoverVia = IsInvestigate}

discoverAtYourLocation :: MonadRandom m => Sourceable source => source -> Int -> m Discover
discoverAtYourLocation (toSource -> source) n = do
  did <- getRandom
  pure
    $ Discover
      { discoverId = did
      , discoverCount = n
      , discoverLocation = DiscoverYourLocation
      , discoverSource = source
      , discoverVia = NotInvestigate
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
    , discoverVia = NotInvestigate
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
    -- Games saved before the three-state split carry 'discoverAction' instead
    discoverVia <-
      o .:? "discoverVia" >>= \case
        Just via -> pure via
        Nothing -> do
          mAction <- o .:? "discoverAction"
          pure $ if mAction == Just Investigate then IsInvestigate else NotInvestigate
    discoverThen <- o .:? "discoverThen" .!= []
    pure Discover {..}
