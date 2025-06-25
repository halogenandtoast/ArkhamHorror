module Arkham.Scenarios.TheMidwinterGala.Meta where

import Arkham.Prelude
import Arkham.Scenarios.TheMidwinterGala.Faction

data Tally
  = ManorsWithNoClues
  | GuestAssetsControlled
  | NoSpellboundInPlay
  | Agenda1AOr2A
  | BloodlessManInVictory
  | PaleLanternInVictory
  | DeclanPearceInVictory
  | RivalInVictory
  | DamageBonus
  | HorrorBonus
  | HardMode
  | ExpertMode
  | FactionBonus
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Meta = Meta {ally :: Faction, rival :: Faction, score :: Map Tally Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

