module Arkham.Helpers.Card (
  module Arkham.Card,
  module Arkham.Helpers.Card,
  module Arkham.Helpers.Campaign,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.ActiveCost.Base
import Arkham.Asset.Types
import Arkham.Card
import Arkham.Classes.Entity
import Arkham.Classes.HasAbilities
import Arkham.Classes.Query
import Arkham.Enemy.Types
import {-# SOURCE #-} Arkham.Entities
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Campaign
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Location.Types
import Arkham.Matcher hiding (AssetCard, LocationCard)
import Arkham.Projection
import Arkham.Store

isDiscardable :: Card -> Bool
isDiscardable = not . isWeakness
 where
  isWeakness = \case
    PlayerCard pc -> isJust $ cdCardSubType $ toCardDef pc
    EncounterCard _ -> True -- maybe?
    VengeanceCard _ -> False -- should be an error

getCardPayments :: (HasGame m) => Card -> m (Maybe Payment)
getCardPayments c = do
  costs <- getActiveCosts
  pure $ activeCostPayments <$> find (isCardTarget . activeCostTarget) costs
 where
  isCardTarget = \case
    ForAbility {} -> False
    ForCard _ c' -> toCardId c == toCardId c'
    ForCost c' -> toCardId c == toCardId c'

extendedCardMatch
  :: (HasGame m, IsCard c, Store m Card) => c -> ExtendedCardMatcher -> m Bool
extendedCardMatch (toCard -> c) matcher =
  selectAny (BasicCardMatch (CardWithId (toCardId c)) <> matcher)

class ConvertToCard a where
  convertToCard :: (Store m Card, HasGame m) => a -> m Card

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
  :: forall a m. (CardEntity a, HasGame m, Store m Card) => EntityId a -> m Card
getEntityCard = field (cardField @a)

instance CardEntity Enemy where
  cardField = EnemyCard

instance CardEntity Asset where
  cardField = AssetCard

instance CardEntity Location where
  cardField = LocationCard

getCardField :: (ConvertToCard c, Store m Card, HasGame m) => (CardDef -> a) -> c -> m a
getCardField f c = f . toCardDef <$> convertToCard c

getVictoryPoints
  :: (ConvertToCard c, Store m Card, HasGame m) => c -> m (Maybe Int)
getVictoryPoints c = do
  card <- convertToCard c
  printedVictory <- getPrintedVictoryPoints card
  modifiers' <- getModifiers $ toCardId card
  pure $ foldr applyModifier printedVictory modifiers'
 where
  applyModifier (GainVictory n) _ = Just n
  applyModifier _ n = n

getHasVictoryPoints :: (ConvertToCard c, Store m Card, HasGame m) => c -> m Bool
getHasVictoryPoints c = isJust <$> getVictoryPoints c

getPrintedVictoryPoints :: (ConvertToCard c, Store m Card, HasGame m) => c -> m (Maybe Int)
getPrintedVictoryPoints = getCardField cdVictoryPoints

-- To get abilities we convert to some entity in Entities and get all abilities
getCardAbilities :: InvestigatorId -> Card -> [Ability]
getCardAbilities iid c = getAbilities $ addCardEntityWith iid id mempty c

findJustCard :: (Store m Card) => (Card -> Bool) -> m Card
findJustCard cardPred = fromJustNote "invalid card" <$> findCard cardPred

findUniqueCard :: (Store m Card) => CardDef -> m Card
findUniqueCard def = findJustCard (`cardMatch` (cardIs def <> CardIsUnique))
