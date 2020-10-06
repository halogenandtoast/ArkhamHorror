module Arkham.Types.Card.PlayerCard.Cards.EmergencyCache where

import ClassyPrelude

import Arkham.Json
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard.Attrs
import Arkham.Types.ClassSymbol
import Arkham.Types.Trait

newtype EmergencyCache = EmergencyCache Attrs
  deriving newtype (Show, ToJSON, FromJSON)

emergencyCache :: CardId -> EmergencyCache
emergencyCache cardId = EmergencyCache
  (event cardId "01088" "Emergency Cache" 0 Neutral) { pcTraits = [Supply] }
