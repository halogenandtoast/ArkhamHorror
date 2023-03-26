module Arkham.Helpers.Card
  ( module Arkham.Helpers.Card
  , module Arkham.Helpers.Campaign
  ) where

import Arkham.Prelude

import Arkham.ActiveCost.Base
import Arkham.Asset.Types
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.Query
import Arkham.Cost
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign
import Arkham.Id
import Arkham.Location.Types
import Arkham.Matcher hiding ( AssetCard, LocationCard )
import Arkham.Projection

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

extendedCardMatch
  :: (HasGame m, IsCard c) => c -> ExtendedCardMatcher -> m Bool
extendedCardMatch (toCard -> c) matcher =
  selectAny (BasicCardMatch (CardWithId (toCardId c)) <> matcher)

class ConvertToCard a where
  convertToCard :: HasGame m => a -> m Card

instance ConvertToCard EnemyId where
  convertToCard = getEntityCard @Enemy

instance ConvertToCard AssetId where
  convertToCard = getEntityCard @Asset

instance ConvertToCard LocationId where
  convertToCard = getEntityCard @Location

instance ConvertToCard Card where
  convertToCard = pure

instance ConvertToCard CardId where
  convertToCard = getCard

class (Projection a, Entity a) => CardEntity a where
  cardField :: Field a Card

getEntityCard
  :: forall a m . (CardEntity a, HasGame m) => EntityId a -> m Card
getEntityCard = field (cardField @a)

instance CardEntity Enemy where
  cardField = EnemyCard

instance CardEntity Asset where
  cardField = AssetCard

instance CardEntity Location where
  cardField = LocationCard

getCardField :: (ConvertToCard c, HasGame m) => (CardDef -> a) -> c -> m a
getCardField f c = f . toCardDef <$> convertToCard c

getVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m (Maybe Int)
getVictoryPoints = getCardField cdVictoryPoints

getHasVictoryPoints :: (ConvertToCard c, HasGame m) => c -> m Bool
getHasVictoryPoints c = isJust <$> getVictoryPoints c
