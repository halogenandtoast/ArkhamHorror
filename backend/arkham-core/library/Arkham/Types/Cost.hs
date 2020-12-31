module Arkham.Types.Cost
  ( module Arkham.Types.Cost
  )
where

import Arkham.Prelude

import Arkham.Types.Action
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Trait

data Cost
  = ActionCost Int (Maybe Action) (HashSet Trait)
  | DiscardCost Int (Maybe PlayerCardType) (HashSet Trait)
  | ClueCost Int
  | ResourceCost Int
  | Costs [Cost]
