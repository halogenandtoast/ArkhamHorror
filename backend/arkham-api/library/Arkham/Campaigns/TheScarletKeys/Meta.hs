{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Meta where

import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Prelude
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

data MapLocationId
  = Alexandria
  | Anchorage
  | Arkham
  | Bermuda
  | BermudaTriangle
  | Bombay
  | BuenosAires
  | Cairo
  | Constantinople
  | Havana
  | HongKong
  | Kabul
  | Kathmandu
  | KualaLumpur
  | Lagos
  | London
  | Manokwari
  | Marrakesh
  | MonteCarlo
  | Moscow
  | Nairobi
  | NewOrleans
  | Perth
  | Quito
  | Reykjavik
  | RioDeJaneiro
  | Rome
  | SanFrancisco
  | SanJuan
  | Shanghai
  | Stockholm
  | Sydney
  | Tokyo
  | Tunguska
  | Venice
  | YborCity
  deriving stock (Show, Eq, Ord, Bounded, Enum, Generic, Data)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

greenLocations :: [MapLocationId]
greenLocations = [Arkham, Cairo, Venice, NewOrleans, MonteCarlo]

data MapLocationType = Standard | Locked | Side
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data MapLocation = MapLocation
  { mapLocationId :: MapLocationId
  , mapLocationType :: MapLocationType
  , mapLocationConnections :: [MapLocationId]
  }
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

type WorldMap = Map MapLocationId MapLocation

worldMap :: Map MapLocationId MapLocation
worldMap =
  mapFromList
    [ (Alexandria, MapLocation Alexandria Standard [Cairo, Constantinople, Rome])
    , (Anchorage, MapLocation Anchorage Standard [SanFrancisco, Arkham])
    , (Arkham, MapLocation Arkham Side [Anchorage, Bermuda, YborCity, Reykjavik])
    , (Bermuda, MapLocation Bermuda Standard [Arkham, YborCity, SanJuan, London, BermudaTriangle])
    , (BermudaTriangle, MapLocation BermudaTriangle Locked [YborCity, Bermuda, SanJuan])
    , (Bombay, MapLocation Bombay Standard [Kabul, Cairo, Kathmandu, KualaLumpur])
    , (BuenosAires, MapLocation BuenosAires Standard [Quito, Sydney, RioDeJaneiro])
    , (Cairo, MapLocation Cairo Side [Alexandria, Lagos, Nairobi, Kabul, Bombay])
    , (Constantinople, MapLocation Constantinople Standard [Venice, Rome, Alexandria, Moscow, Kabul])
    , (Havana, MapLocation Havana Standard [SanFrancisco, YborCity, Quito, SanJuan])
    , (HongKong, MapLocation HongKong Locked [Shanghai, KualaLumpur])
    , (Kabul, MapLocation Kabul Locked [Constantinople, Moscow, Tunguska, Kathmandu, Bombay, Cairo])
    , (Kathmandu, MapLocation Kathmandu Standard [Kabul, Tunguska, Shanghai, KualaLumpur, Bombay])
    , (KualaLumpur, MapLocation KualaLumpur Locked [Bombay, Kathmandu, HongKong, Manokwari, Perth])
    , (Lagos, MapLocation Lagos Standard [Marrakesh, Cairo, Nairobi, RioDeJaneiro])
    , (London, MapLocation London Locked [SanJuan, Bermuda, Stockholm, Venice, MonteCarlo])
    , (Manokwari, MapLocation Manokwari Locked [KualaLumpur, Sydney, Shanghai, Quito])
    , (Marrakesh, MapLocation Marrakesh Standard [SanJuan, MonteCarlo, Rome, Lagos])
    , (MonteCarlo, MapLocation MonteCarlo Side [London, Marrakesh, Rome])
    , (Moscow, MapLocation Moscow Standard [Constantinople, Tunguska, Kabul, Stockholm, Venice])
    , (Nairobi, MapLocation Nairobi Standard [Cairo, Lagos, Perth])
    , (NewOrleans, MapLocation NewOrleans Side [SanFrancisco, YborCity])
    , (Perth, MapLocation Perth Standard [KualaLumpur, Sydney, Nairobi])
    , (Quito, MapLocation Quito Locked [Havana, Manokwari, BuenosAires, SanJuan])
    , (Reykjavik, MapLocation Reykjavik Locked [Arkham, Stockholm])
    , (RioDeJaneiro, MapLocation RioDeJaneiro Standard [BuenosAires, Lagos, SanJuan])
    , (Rome, MapLocation Rome Standard [Alexandria, Constantinople, Venice, MonteCarlo, Marrakesh])
    , (SanFrancisco, MapLocation SanFrancisco Standard [Anchorage, Havana, NewOrleans, Tokyo])
    ,
      ( SanJuan
      , MapLocation
          SanJuan
          Locked
          [Bermuda, BermudaTriangle, YborCity, Havana, Quito, RioDeJaneiro, London, Marrakesh]
      )
    , (Shanghai, MapLocation Shanghai Standard [Kathmandu, Tunguska, Tokyo, HongKong, Manokwari])
    , (Stockholm, MapLocation Stockholm Standard [London, Reykjavik, Moscow])
    , (Sydney, MapLocation Sydney Standard [BuenosAires, Manokwari, Perth])
    , (Tokyo, MapLocation Tokyo Standard [SanFrancisco, Shanghai])
    , (Tunguska, MapLocation Tunguska Locked [Moscow, Kabul, Kathmandu, Shanghai])
    , (Venice, MapLocation Venice Side [London, Constantinople, Moscow, Rome])
    ,
      ( YborCity
      , MapLocation YborCity Standard [NewOrleans, Arkham, Bermuda, SanJuan, BermudaTriangle, Havana]
      )
    ]

data KeyStatus
  = KeyWithInvestigator InvestigatorId
  | KeyWithEnemy CardCode
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

keyWithEnemy :: HasCardCode enemy => enemy -> KeyStatus
keyWithEnemy = KeyWithEnemy . toCardCode

initMeta :: TheScarletKeysMeta
initMeta = TheScarletKeysMeta [London] initUnlockedLocations mempty worldMap London [] Nothing Nothing

initUnlockedLocations :: [MapLocationId]
initUnlockedLocations =
  [ Anchorage
  , SanFrancisco
  , YborCity
  , Havana
  , BuenosAires
  , RioDeJaneiro
  , Bermuda
  , Stockholm
  , Moscow
  , Rome
  , Constantinople
  , Marrakesh
  , Lagos
  , Nairobi
  , Alexandria
  , Bombay
  , Kathmandu
  , Shanghai
  , Tokyo
  , Perth
  , Sydney
  ]
    <> greenLocations

mapDistance :: TheScarletKeysMeta -> MapLocationId -> Maybe Int
mapDistance meta = mapDistance' meta.campaignMap meta.currentLocation

mapDistance'
  :: WorldMap
  -> MapLocationId
  -- ^ Starting location
  -> MapLocationId
  -- ^ Destination location
  -> Maybe Int
  -- ^ Distance in hops (0 if same location)
mapDistance' world start goal
  | start == goal = Just 0
  | otherwise = bfs Set.empty [(start, 0)]
 where
  bfs _ [] = Nothing
  bfs visited ((loc, dist) : rest)
    | loc == goal = Just dist
    | Set.member loc visited = bfs visited rest
    | otherwise =
        let neighbors = maybe [] mapLocationConnections (Map.lookup loc world)
            newQueue =
              rest
                ++ [(n, dist + if n `elem` greenLocations then 0 else 1) | n <- neighbors, Set.notMember n visited]
         in bfs (Set.insert loc visited) newQueue

data TheScarletKeysMeta = TheScarletKeysMeta
  { visitedLocations :: [MapLocationId]
  , unlockedLocations :: [MapLocationId]
  , keyStatus :: Map CardCode KeyStatus
  , campaignMap :: WorldMap
  , currentLocation :: MapLocationId
  , canReset :: [MapLocationId]
  , theta :: Maybe Int
  , psi :: Maybe Int
  }
  deriving stock (Show, Eq, Ord, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

$(makeLensesWith lFields ''TheScarletKeysMeta)
