module Arkham.Types.Card
  ( module Arkham.Types.Card
  , module X
  ) where

import Arkham.Prelude

import Arkham.Types.Card.CardCode as X
import Arkham.Types.Card.CardDef as X
import Arkham.Types.Card.CardType as X
import Arkham.Types.Card.Class as X
import Arkham.Types.Card.Cost
import Arkham.Types.Card.EncounterCard
import Arkham.Types.Card.EncounterCard as X (EncounterCard(..))
import Arkham.Types.Card.Id
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Card.PlayerCard as X
  (BearerId(..), DiscardedPlayerCard(..), PlayerCard(..))
import Arkham.Types.InvestigatorId
import Arkham.Types.Name

data CardBuilder ident a = CardBuilder
  { cbCardCode :: CardCode
  , cbCardBuilder :: ident -> a
  }

instance Functor (CardBuilder ident) where
  fmap f CardBuilder {..} =
    CardBuilder { cbCardCode = cbCardCode, cbCardBuilder = f . cbCardBuilder }

newtype SetAsideCard = SetAsideCard { unSetAsideCard :: Card }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype CommittedCard = CommittedCard { unCommittedCard :: Card }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

_PlayerCard :: Traversal' Card PlayerCard
_PlayerCard f (PlayerCard pc) = PlayerCard <$> f pc
_PlayerCard _ other = pure other

instance Named Card where
  toName = toName . toCardDef

instance HasCardDef Card where
  toCardDef = \case
    PlayerCard pc -> toCardDef pc
    EncounterCard ec -> toCardDef ec

instance HasCardCode Card where
  toCardCode = \case
    PlayerCard pc -> toCardCode pc
    EncounterCard ec -> toCardCode ec

instance HasOriginalCardCode Card where
  toOriginalCardCode = \case
    PlayerCard pc -> toOriginalCardCode pc
    EncounterCard ec -> toOriginalCardCode ec

data CampaignStoryCard = CampaignStoryCard
  { campaignStoryCardInvestigatorId :: InvestigatorId
  , campaignStoryCardPlayerCard :: PlayerCard
  }

newtype DeckCard = DeckCard { unDeckCard ::PlayerCard }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype HandCard = HandCard { unHandCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype UnderneathCard = UnderneathCard { unUnderneathCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype DiscardableHandCard = DiscardableHandCard { unDiscardableHandCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype PlayableHandCard = PlayableHandCard { unPlayableHandCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype InPlayCard = InPlayCard { unInPlayCard ::Card }
  deriving stock (Show, Generic)
  deriving newtype (ToJSON, FromJSON)

class HasCard env a where
  getCard :: (MonadReader env m, MonadIO m) => CardId -> a -> m Card

instance HasSkillIcons Card where
  getSkillIcons (PlayerCard card) = getSkillIcons card
  getSkillIcons (EncounterCard _) = []

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost (EncounterCard _) = 0

isDynamic :: Card -> Bool
isDynamic (PlayerCard card) = case cdCost (toCardDef card) of
  Just DynamicCost -> True
  _ -> False
isDynamic (EncounterCard _) = False

toPlayerCard :: Card -> Maybe PlayerCard
toPlayerCard (PlayerCard pc) = Just pc
toPlayerCard (EncounterCard _) = Nothing

toEncounterCard :: Card -> Maybe EncounterCard
toEncounterCard (EncounterCard ec) = Just ec
toEncounterCard (PlayerCard _) = Nothing

cardIsWeakness :: Card -> Bool
cardIsWeakness (EncounterCard _) = False
cardIsWeakness (PlayerCard pc) = cdWeakness (toCardDef pc)

filterCardType :: HasCardDef a => CardType -> [a] -> [a]
filterCardType cardType' = filter ((== cardType') . cdCardType . toCardDef)

filterLocations :: HasCardDef a => [a] -> [a]
filterLocations = filterCardType LocationType
