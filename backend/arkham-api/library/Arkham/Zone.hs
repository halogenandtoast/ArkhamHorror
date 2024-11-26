{-# LANGUAGE TemplateHaskell #-}

module Arkham.Zone where

import Arkham.Prelude

import Data.Aeson.TH
import Data.Aeson.Types
import Data.Proxy
import GHC.OverloadedLabels

data OutOfPlayZone
  = VoidZone
  | PursuitZone
  | SetAsideZone
  | VictoryDisplayZone
  | RemovedZone
  | TheDepths
  deriving stock (Show, Eq, Ord, Enum, Bounded, Data)

overOutOfPlayZones :: (forall a. KnownOutOfPlayZone a => Proxy a -> b) -> [b]
overOutOfPlayZones f = map (\(SomeZone p) -> f p) someZones

class KnownOutOfPlayZone a where
  knownOutOfPlayZone :: Proxy (a :: OutOfPlayZone) -> OutOfPlayZone

instance KnownOutOfPlayZone 'VoidZone where
  knownOutOfPlayZone _ = VoidZone

instance KnownOutOfPlayZone 'PursuitZone where
  knownOutOfPlayZone _ = PursuitZone

instance KnownOutOfPlayZone 'SetAsideZone where
  knownOutOfPlayZone _ = SetAsideZone

instance KnownOutOfPlayZone 'VictoryDisplayZone where
  knownOutOfPlayZone _ = VictoryDisplayZone

instance KnownOutOfPlayZone 'RemovedZone where
  knownOutOfPlayZone _ = RemovedZone

instance KnownOutOfPlayZone 'TheDepths where
  knownOutOfPlayZone _ = TheDepths

data SomeZone where
  SomeZone :: KnownOutOfPlayZone zone => Proxy zone -> SomeZone

someZones :: [SomeZone]
someZones =
  [ SomeZone (Proxy @'VoidZone)
  , SomeZone (Proxy @'PursuitZone)
  , SomeZone (Proxy @'SetAsideZone)
  , SomeZone (Proxy @'VictoryDisplayZone)
  , SomeZone (Proxy @'RemovedZone)
  , SomeZone (Proxy @'TheDepths)
  ]

data Zone
  = FromHand
  | FromDeck
  | FromTopOfDeck Int
  | FromBottomOfDeck Int
  | FromDiscard
  | FromPlay
  | FromOutOfPlay OutOfPlayZone
  | FromCollection
  deriving stock (Show, Eq, Ord, Data)

zoneIsFromDeck :: Zone -> Bool
zoneIsFromDeck = \case
  FromDeck -> True
  FromTopOfDeck _ -> True
  FromBottomOfDeck _ -> True
  _ -> False

data ScenarioZone
  = FromEncounterDeck
  | FromEncounterDiscard
  | FromOutOfPlayArea OutOfPlayZone
  deriving stock (Show, Eq, Ord, Data)

pattern FromVoid :: ScenarioZone
pattern FromVoid <- FromOutOfPlayArea VoidZone
  where
    FromVoid = FromOutOfPlayArea VoidZone

instance IsLabel "deck" ScenarioZone where
  fromLabel = FromEncounterDeck

instance IsLabel "discard" ScenarioZone where
  fromLabel = FromEncounterDiscard

instance IsLabel "void" ScenarioZone where
  fromLabel = FromVoid

allOutOfPlayZones :: [ScenarioZone]
allOutOfPlayZones = map FromOutOfPlayArea [minBound .. maxBound]

$(deriveJSON defaultOptions ''OutOfPlayZone)
$(deriveJSON defaultOptions ''Zone)
$(deriveJSON defaultOptions ''ScenarioZone)

instance ToJSONKey Zone where
  toJSONKey = toJSONKeyText textKey
   where
    textKey = \case
      FromTopOfDeck _ -> "FromDeck"
      FromBottomOfDeck _ -> "FromDeck"
      FromOutOfPlay outOfPlayZone -> case outOfPlayZone of
        VoidZone -> "FromVoid"
        PursuitZone -> "FromPursuit"
        SetAsideZone -> "FromSetAside"
        VictoryDisplayZone -> "FromVictoryDisplay"
        RemovedZone -> "RemovedZone"
        TheDepths -> "TheDepths"
      other -> tshow other

instance FromJSONKey Zone where
  fromJSONKey = FromJSONKeyText \case
    "FromHand" -> FromHand
    "FromDeck" -> FromDeck
    "FromDiscard" -> FromDiscard
    "FromPlay" -> FromPlay
    "FromVoid" -> FromOutOfPlay VoidZone
    "FromPursuit" -> FromOutOfPlay PursuitZone
    "FromSetAside" -> FromOutOfPlay SetAsideZone
    "FromVictoryDisplay" -> FromOutOfPlay VictoryDisplayZone
    "FromCollection" -> FromCollection
    other -> error ("Unhandled FromJSONKey for zone" <> show other)

zoneLabel :: Zone -> Text
zoneLabel = \case
  FromHand -> "Hand"
  FromDeck -> "Deck"
  FromTopOfDeck n -> "Top " <> tshow n <> " Card of Deck"
  FromBottomOfDeck n -> "Bottom " <> tshow n <> "Cards of Deck"
  FromDiscard -> "Discard"
  FromPlay -> "Play"
  FromOutOfPlay _ -> "Out of Play"
  FromCollection -> "Collection"

instance ToJSONKey OutOfPlayZone
instance FromJSONKey OutOfPlayZone
