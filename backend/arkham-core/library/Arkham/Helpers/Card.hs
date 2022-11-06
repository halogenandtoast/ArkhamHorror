module Arkham.Helpers.Card
  ( module Arkham.Helpers.Card
  , module Arkham.Helpers.Campaign
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Cost
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?

getCardPayments :: Card -> GameT (Maybe Payment)
getCardPayments c = do
  costs <- getActiveCosts
  let mcost = find (isCardTarget . activeCostTarget) costs
  pure $ case mcost of
    Just cost -> Just $ activeCostPayments cost
    Nothing -> Nothing
 where
   isCardTarget = \case
    ForAbility{} -> False
    ForCard _ c' -> toCardId c == toCardId c'
    ForCost c' -> toCardId c == toCardId c'

