module Arkham.Card
  ( module Arkham.Card
  , module X
  ) where

import Arkham.Prelude

import Arkham.Card.CardCode as X
import Arkham.Card.CardDef as X
import Arkham.Card.CardType as X
import Arkham.Card.Class as X
import Arkham.Card.Id as X
import Arkham.Card.EncounterCard as X ( EncounterCard (..) )
import Arkham.Card.PlayerCard as X ( PlayerCard (..) )

import Arkham.Card.Cost
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes.GameLogger
import Arkham.EncounterCard
import Arkham.Id
import Arkham.Matcher
import Arkham.Name
import Arkham.PlayerCard
import Arkham.SkillType
import Arkham.Trait
import Data.Text qualified as T

lookupCard
  :: (HasCallStack, HasCardCode cardCode) => cardCode -> CardId -> Card
lookupCard (toCardCode -> cardCode) cardId =
  case (lookup cardCode allEncounterCards, lookup cardCode allPlayerCards) of
    (Nothing, Nothing) -> error $ "Missing card " <> show cardCode
    (Just def, _) -> EncounterCard $ lookupEncounterCard def cardId
    -- we prefer encounter cards over player cards to handle cases like straitjacket
    (Nothing, Just def) -> PlayerCard $ lookupPlayerCard def cardId

-- we prefer encounter cards over player cards to handle cases like straitjacket
lookupCardDef :: (HasCardCode cardCode) => cardCode -> Maybe CardDef
lookupCardDef (toCardCode -> cardCode) = lookup cardCode allEncounterCards <|> lookup cardCode allPlayerCards

data CardBuilder ident a = CardBuilder
  { cbCardCode :: CardCode
  , cbCardBuilder :: CardId -> ident -> a
  }

instance Functor (CardBuilder ident) where
  fmap f CardBuilder {..} =
    CardBuilder { cbCardCode = cbCardCode, cbCardBuilder = \cId -> f . cbCardBuilder cId }

instance IsCard Card where
  toCardId = \case
    PlayerCard pc -> toCardId pc
    EncounterCard ec -> toCardId ec
    VengeanceCard c -> toCardId c
  toCardOwner = \case
    PlayerCard pc -> toCardOwner pc
    EncounterCard ec -> toCardOwner ec
    VengeanceCard c -> toCardOwner c

class (HasTraits a, HasCardDef a, HasCardCode a) => IsCard a where
  toCard :: HasCallStack => a -> Card
  toCard a = case lookupCard (cdCardCode $ toCardDef a) (toCardId a) of
    PlayerCard pc -> PlayerCard $ pc { pcOwner = toCardOwner a }
    ec -> ec
  toCardId :: a -> CardId
  toCardOwner :: a -> Maybe InvestigatorId

class MonadRandom m => CardGen m where
  genEncounterCard :: HasCardDef a => a -> m EncounterCard
  genPlayerCard :: HasCardDef a => a -> m PlayerCard

instance CardGen IO where
  genEncounterCard a = lookupEncounterCard (toCardDef a) <$> getRandom
  genPlayerCard a = lookupPlayerCard (toCardDef a) <$> getRandom

genCard :: (HasCardDef a, CardGen m) => a -> m Card
genCard a = if cdCardType def `elem` encounterCardTypes
  then EncounterCard <$> genEncounterCard def
  else PlayerCard <$> genPlayerCard def
  where def = toCardDef a

cardMatch :: IsCard a => a -> CardMatcher -> Bool
cardMatch a = \case
  AnyCard -> True
  IsEncounterCard -> toCardType a `elem` encounterCardTypes
  CardIsUnique -> cdUnique $ toCardDef a
  CardWithType cardType' -> toCardType a == cardType'
  CardWithSkillIcon skillIcon ->
    skillIcon `member` setFromList @(HashSet SkillIcon) (cdSkills $ toCardDef a)
  CardWithCardCode cardCode -> toCardCode a == cardCode
  CardWithId cardId -> toCardId a == cardId
  CardWithTitle title -> (nameTitle . cdName $ toCardDef a) == title
  CardWithTrait trait -> trait `member` toTraits a
  CardWithClass role -> role `member` cdClassSymbols (toCardDef a)
  CardWithLevel n -> cdLevel (toCardDef a) == n
  FastCard -> isJust $ cdFastWindow (toCardDef a)
  CardMatches ms -> all (cardMatch a) ms
  CardWithVengeance -> isJust . cdVengeancePoints $ toCardDef a
  CardWithOneOf ms -> any (cardMatch a) ms
  CardWithoutKeyword k -> k `notMember` cdKeywords (toCardDef a)
  NonWeakness -> isNothing . cdCardSubType $ toCardDef a
  WeaknessCard -> isJust . cdCardSubType $ toCardDef a
  NonExceptional -> not . cdExceptional $ toCardDef a
  NotCard m -> not (cardMatch a m)
  CardWithPrintedLocationSymbol sym ->
    (== Just sym) . cdLocationRevealedSymbol $ toCardDef a
  CardWithPrintedLocationConnection sym ->
    elem sym . cdLocationRevealedConnections $ toCardDef a
  CardFillsSlot slot -> elem slot $ cdSlots $ toCardDef a
  DiscardableCard -> cardMatch a NonWeakness

instance IsCard PlayerCard where
  toCardId = pcId
  toCardOwner = pcOwner

instance IsCard EncounterCard where
  toCardId = ecId
  toCardOwner = const Nothing

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  | VengeanceCard Card
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Eq Card where
  a == b = toCardId a == toCardId b

flipCard :: Card -> Card
flipCard (EncounterCard ec) =
  EncounterCard $ ec { ecIsFlipped = not <$> ecIsFlipped ec }
flipCard (PlayerCard pc) = PlayerCard pc
flipCard (VengeanceCard c) = VengeanceCard c

_PlayerCard :: Traversal' Card PlayerCard
_PlayerCard f (PlayerCard pc) = PlayerCard <$> f pc
_PlayerCard _ other = pure other

_EncounterCard :: Traversal' Card EncounterCard
_EncounterCard f (EncounterCard pc) = EncounterCard <$> f pc
_EncounterCard _ other = pure other

instance Named Card where
  toName = toName . toCardDef

instance HasCardDef Card where
  toCardDef = \case
    PlayerCard pc -> toCardDef pc
    EncounterCard ec -> toCardDef ec
    VengeanceCard c -> toCardDef c

instance HasCardCode Card where
  toCardCode = \case
    PlayerCard pc -> toCardCode pc
    EncounterCard ec -> toCardCode ec
    VengeanceCard c -> toCardCode c

instance HasOriginalCardCode Card where
  toOriginalCardCode = \case
    PlayerCard pc -> toOriginalCardCode pc
    EncounterCard ec -> toOriginalCardCode ec
    VengeanceCard c -> toOriginalCardCode c

data CampaignStoryCard = CampaignStoryCard
  { campaignStoryCardInvestigatorId :: InvestigatorId
  , campaignStoryCardPlayerCard :: PlayerCard
  }

class HasCard env a where
  getCard :: (MonadReader env m, MonadIO m) => CardId -> a -> m Card

instance HasSkillIcons Card where
  getSkillIcons (PlayerCard card) = getSkillIcons card
  getSkillIcons (EncounterCard _) = []
  getSkillIcons (VengeanceCard _) = []

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost (EncounterCard _) = 0
  getCost (VengeanceCard _) = 0

isDynamic :: Card -> Bool
isDynamic (PlayerCard card) = case cdCost (toCardDef card) of
  Just DynamicCost -> True
  _ -> False
isDynamic (EncounterCard _) = False
isDynamic (VengeanceCard _) = False

isFastCard :: Card -> Bool
isFastCard (PlayerCard card) =
  let CardDef {..} = toCardDef card in isJust cdFastWindow
isFastCard (EncounterCard _) = False
isFastCard (VengeanceCard _) = False

toPlayerCard :: Card -> Maybe PlayerCard
toPlayerCard (PlayerCard pc) = Just pc
toPlayerCard (EncounterCard _) = Nothing
toPlayerCard (VengeanceCard c) = toPlayerCard c

toEncounterCard :: Card -> Maybe EncounterCard
toEncounterCard (EncounterCard ec) = Just ec
toEncounterCard (PlayerCard _) = Nothing
toEncounterCard (VengeanceCard _) = Nothing

cardIsWeakness :: Card -> Bool
cardIsWeakness (EncounterCard _) = False
cardIsWeakness (PlayerCard pc) = isJust $ cdCardSubType (toCardDef pc)
cardIsWeakness (VengeanceCard _) = False

filterCardType :: HasCardDef a => CardType -> [a] -> [a]
filterCardType cardType' = filter ((== cardType') . cdCardType . toCardDef)

filterLocations :: HasCardDef a => [a] -> [a]
filterLocations = filterCardType LocationType

instance ToGameLoggerFormat Card where
  format c =
    "{card:\""
      <> T.replace "\"" "\\\"" (display $ toName c)
      <> "\":"
      <> tshow (toCardCode c)
      <> ":\""
      <> tshow (toCardId c)
      <> "\"}"
