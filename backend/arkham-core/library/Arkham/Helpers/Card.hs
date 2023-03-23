module Arkham.Helpers.Card
  ( module Arkham.Helpers.Card
  , module Arkham.Helpers.Campaign
  ) where

import Arkham.Prelude

import Arkham.Classes.Query
import Arkham.Card
import Arkham.Cost
import Arkham.Matcher
import Arkham.ActiveCost.Base
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
    VengeanceCard _ -> False -- should be an error

getCardPayments :: HasGame m => Card -> m (Maybe Payment)
getCardPayments c = do
  costs <- getActiveCosts
  pure $ activeCostPayments <$> find (isCardTarget . activeCostTarget) costs
 where
   isCardTarget = \case
    ForAbility{} -> False
    ForCard _ c' -> toCardId c == toCardId c'
    ForCost c' -> toCardId c == toCardId c'

extendedCardMatch :: (HasGame m, IsCard c) => c -> ExtendedCardMatcher -> m Bool
extendedCardMatch (toCard -> c) matcher = selectAny (BasicCardMatch (CardWithId (toCardId c)) <> matcher)
