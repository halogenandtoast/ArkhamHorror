{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card (
  module Arkham.Card,
  module X,
) where

import Arkham.Prelude

import Arkham.Card.CardCode as X
import Arkham.Card.CardDef as X
import Arkham.Card.CardType as X
import Arkham.Card.Class as X
import Arkham.Card.Cost as X
import Arkham.Card.EncounterCard as X (EncounterCard (..))
import Arkham.Card.Id as X
import Arkham.Card.PlayerCard as X (PlayerCard (..))

import Arkham.Action (Action)
import Arkham.Calculation
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes.GameLogger
import Arkham.Customization
import Arkham.EncounterCard
import Arkham.Enemy.Cards (allSpecialEnemyCards)
import Arkham.Id
import Arkham.Keyword (Keyword (Hidden, Peril))
import Arkham.Matcher
import Arkham.Name
import Arkham.PlayerCard
import Arkham.Queue
import Arkham.SkillType
import Arkham.Taboo.Types
import Arkham.Trait
import Control.Monad.State.Strict (StateT)
import Control.Monad.Writer.Strict (WriterT)
import Data.Aeson.TH
import Data.Text qualified as T
import GHC.Records

lookupCard
  :: (HasCallStack, HasCardCode cardCode) => cardCode -> CardId -> Card
lookupCard (toCardCode -> cardCode) cardId =
  case (lookup cardCode allEncounterCards, lookup cardCode (allPlayerCards <> allSpecialEnemyCards)) of
    (Nothing, Nothing) -> error $ "Missing card " <> show cardCode
    (Just def, _) -> EncounterCard $ lookupEncounterCard def cardId
    -- we prefer encounter cards over player cards to handle cases like straitjacket
    (Nothing, Just def) -> PlayerCard $ lookupPlayerCard def cardId

-- we prefer encounter cards over player cards to handle cases like straitjacket
lookupCardDef :: HasCardCode cardCode => cardCode -> Maybe CardDef
lookupCardDef (toCardCode -> cardCode) =
  lookup cardCode allEncounterCards <|> lookup cardCode allPlayerCards

data CardBuilder ident a = CardBuilder
  { cbCardCode :: CardCode
  , cbCardBuilder :: CardId -> ident -> a
  }

instance HasField "cardCode" (CardBuilder ident a) CardCode where
  getField = toCardCode

instance HasCardCode (CardBuilder ident a) where
  toCardCode = cbCardCode

instance Functor (CardBuilder ident) where
  fmap f CardBuilder {..} =
    CardBuilder
      { cbCardCode = cbCardCode
      , cbCardBuilder = \cId -> f . cbCardBuilder cId
      }

instance IsCard Card where
  toCard = id
  toCardId = \case
    PlayerCard pc -> toCardId pc
    EncounterCard ec -> toCardId ec
    VengeanceCard c -> toCardId c
  toCardOwner = \case
    PlayerCard pc -> toCardOwner pc
    EncounterCard ec -> toCardOwner ec
    VengeanceCard c -> toCardOwner c

-- WARNING: toCard has a default, but we should only use this if the original
-- card is not recoverable
--
defaultToCard :: (HasCallStack, IsCard a) => a -> Card
defaultToCard a = case lookupCard (cdCardCode $ toCardDef a) (toCardId a) of
  PlayerCard pc -> PlayerCard $ pc {pcOwner = toCardOwner a, pcCustomizations = toCustomizations a}
  ec -> ec

class (HasTraits a, HasCardDef a, HasCardCode a) => IsCard a where
  toCard :: HasCallStack => a -> Card
  toCardId :: a -> CardId
  toCardOwner :: a -> Maybe InvestigatorId
  toCustomizations :: a -> Customizations
  toCustomizations _ = mempty

toCards :: (IsCard a, Functor f) => f a -> f Card
toCards = fmap toCard

sameCard :: (IsCard a, IsCard b) => a -> b -> Bool
sameCard a b = toCardId a == toCardId b

cardIds :: IsCard a => [a] -> [CardId]
cardIds = map toCardId

class MonadRandom m => CardGen m where
  genEncounterCard :: HasCardDef a => a -> m EncounterCard
  genPlayerCard :: HasCardDef a => a -> m PlayerCard
  replaceCard :: CardId -> Card -> m ()
  removeCard :: CardId -> m ()
  clearCardCache :: m ()

instance (CardGen m, Monoid w) => CardGen (WriterT w m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cardId card = lift $ replaceCard cardId card
  clearCardCache = lift clearCardCache
  removeCard = lift . removeCard

instance CardGen m => CardGen (MaybeT m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cardId card = lift $ replaceCard cardId card
  clearCardCache = lift clearCardCache
  removeCard = lift . removeCard

instance CardGen m => CardGen (StateT s m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cardId card = lift $ replaceCard cardId card
  clearCardCache = lift clearCardCache
  removeCard = lift . removeCard

instance CardGen m => CardGen (ReaderT r m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cardId card = lift $ replaceCard cardId card
  clearCardCache = lift clearCardCache
  removeCard = lift . removeCard

instance CardGen m => CardGen (QueueT msg m) where
  genEncounterCard = lift . genEncounterCard
  genPlayerCard = lift . genPlayerCard
  replaceCard cardId card = lift $ replaceCard cardId card
  clearCardCache = lift clearCardCache
  removeCard = lift . removeCard

overPlayerCard :: (PlayerCard -> PlayerCard) -> Card -> Card
overPlayerCard f = \case
  PlayerCard pc -> PlayerCard $ f pc
  other -> other

genPlayerCardWith :: (HasCardDef a, CardGen m) => a -> (PlayerCard -> PlayerCard) -> m PlayerCard
genPlayerCardWith a f = do
  result <- f <$> genPlayerCard a
  replaceCard (toCardId result) (PlayerCard result)
  pure result

-- instance CardGen IO where
--   genEncounterCard a =
--     lookupEncounterCard (toCardDef a) . unsafeMakeCardId <$> getRandom
--   genPlayerCard a =
--     lookupPlayerCard (toCardDef a) . unsafeMakeCardId <$> getRandom
--   replaceCard _ _ = pure ()

genCard :: (HasCardDef a, CardGen m) => a -> m Card
genCard a =
  if cdCardType def `elem` encounterCardTypes
    then EncounterCard <$> genEncounterCard def
    else PlayerCard <$> genPlayerCard def
 where
  def = toCardDef a

genFlippedCard :: (HasCardDef a, CardGen m) => a -> m Card
genFlippedCard a = flipCard <$> genCard a

genCards :: (HasCardDef a, CardGen m, Traversable t) => t a -> m (t Card)
genCards = traverse genCard

genSetAsideCards :: (HasCardDef a, CardGen m) => [a] -> m [Card]
genSetAsideCards cards = traverse genCard $ concatMap splay cards
 where
  splay card = replicate (fromMaybe 0 $ cdEncounterSetQuantity $ toCardDef card) card

genPlayerCards :: (HasCardDef a, CardGen m, Traversable t) => t a -> m (t PlayerCard)
genPlayerCards = traverse genPlayerCard

isCard :: (HasCardCode a, HasCardCode b) => a -> b -> Bool
isCard (toCardCode -> a) (toCardCode -> b) = a == b

printedCardCost :: IsCard a => a -> Int
printedCardCost = maybe 0 toPrintedCost . cdCost . toCardDef

cardMatch :: (IsCard a, IsCardMatcher cardMatcher, HasCallStack) => a -> cardMatcher -> Bool
cardMatch a (toCardMatcher -> cardMatcher) = case cardMatcher of
  AnyCard -> True
  CardTaggedWith tx -> tx `elem` cdTags (toCardDef a)
  CardWithAvailableCustomization -> case toCard a of
    PlayerCard pc ->
      let customizations = cdCustomizations $ toCardDef a
       in not $ all (hasCustomization_ customizations (pcCustomizations pc)) (keys customizations)
    _ -> False
  CardWithOddCost -> maybe False (odd . toPrintedCost) (cdCost $ toCardDef a)
  CardWithNonZeroCost -> maybe False ((> 0) . toPrintedCost) (cdCost $ toCardDef a)
  CardWithEvenCost -> maybe False (even . toPrintedCost) (cdCost $ toCardDef a)
  CardWithCost n -> maybe False ((== n) . toPrintedCost) (cdCost $ toCardDef a)
  CardWithOddSkillIcons -> odd $ length (cdSkills $ toCardDef a)
  CardWithEvenSkillIcons -> even $ length (cdSkills $ toCardDef a)
  CardWithNoSkills -> null (cdSkills $ toCardDef a)
  CardWithAnySkills -> notNull (cdSkills $ toCardDef a)
  CardWithOddNumberOfWordsInTitle -> odd $ length $ words (toTitle $ toCardDef a)
  CardWithEvenNumberOfWordsInTitle -> even $ length $ words (toTitle $ toCardDef a)
  CardFromEncounterSet encounterSet ->
    cdEncounterSet (toCardDef a) == Just encounterSet
  IsEncounterCard -> toCardType a `elem` encounterCardTypes
  IsPlayerCard -> toCardType a `elem` playerCardTypes
  CardIsUnique -> cdUnique $ toCardDef a
  CardWithType cardType' -> toCardType a == cardType'
  CardWithSubType subType' -> cdCardSubType (toCardDef a) == Just subType'
  CardWithSkillIcon skillIcon ->
    skillIcon `member` setFromList @(Set SkillIcon) (cdSkills $ toCardDef a)
  CardWithCardCode cardCode -> toCardCode a == cardCode
  CardWithId cardId -> toCardId a == cardId
  CardWithTitle title -> (nameTitle . cdName $ toCardDef a) == title
  CardWithTrait trait -> trait `member` toTraits a
  CardWithClass role -> role `member` cdClassSymbols (toCardDef a)
  CardWithLevel n -> Just n == cdLevel (toCardDef a)
  CardWithMaxLevel n -> maybe False (<= n) $ cdLevel (toCardDef a)
  FastCard -> isJust $ cdFastWindow (toCardDef a)
  CardMatches ms -> all (cardMatch a) ms
  CardWithVengeance -> isJust . cdVengeancePoints $ toCardDef a
  CardWithOneOf ms -> any (cardMatch a) ms
  CardWithoutKeyword Peril -> case toCard a of
    EncounterCard ec -> Peril `notMember` cdKeywords (toCardDef a) && not (ecAddedPeril ec)
    _ -> Peril `notMember` cdKeywords (toCardDef a)
  CardWithoutKeyword k -> k `notMember` cdKeywords (toCardDef a)
  CardWithKeyword k -> k `member` cdKeywords (toCardDef a)
  NonWeakness -> isNothing . cdCardSubType $ toCardDef a
  SignatureCard -> isSignature $ toCardDef a
  BasicWeaknessCard -> (== Just BasicWeakness) . cdCardSubType $ toCardDef a
  WeaknessCard -> isJust . cdCardSubType $ toCardDef a
  NonExceptional -> not . cdExceptional $ toCardDef a
  PermanentCard -> cdPermanent $ toCardDef a
  NotCard m -> not (cardMatch a m)
  CardWithAction action -> elem action $ cdActions $ toCardDef a
  CardWithoutAction -> null $ cdActions $ toCardDef a
  CardWithPrintedLocationSymbol sym ->
    (== Just sym) . cdLocationRevealedSymbol $ toCardDef a
  CardWithPrintedLocationConnection sym ->
    elem sym . cdLocationRevealedConnections $ toCardDef a
  CardFillsSlot slot -> elem slot $ cdSlots $ toCardDef a
  CardFillsLessSlots n slot -> count (== slot) (cdSlots $ toCardDef a) < n
  DiscardableCard -> cardMatch a (NonWeakness <> CardWithoutKeyword Hidden)
  CardWithRevelation -> cdRevelation (toCardDef a) /= NoRevelation
  CardOwnedBy iid -> toCardOwner a == Just iid

isNonWeakness :: IsCard a => a -> Bool
isNonWeakness = (`cardMatch` NonWeakness)

filterCards :: (IsCardMatcher a, IsCard b) => a -> [b] -> [b]
filterCards matcher = filter ((`cardMatch` matcher) . toCard)

findCardMatch
  :: (IsCardMatcher a, IsCard card, Element cards ~ card, MonoFoldable cards)
  => a
  -> cards
  -> Maybe card
findCardMatch matcher = find ((`cardMatch` matcher) . toCard) . toList

card_ :: CardMatcher -> CardMatcher
card_ = id

instance IsCard PlayerCard where
  toCard = PlayerCard
  toCardId = pcId
  toCardOwner = pcOwner

instance IsCard EncounterCard where
  toCard = EncounterCard
  toCardId = ecId
  toCardOwner = ecOwner

setOwner :: CardGen m => InvestigatorId -> Card -> m Card
setOwner iid card = do
  let result = go card
  replaceCard (toCardId result) result
  pure result
 where
  go = \case
    PlayerCard pc -> PlayerCard (pc {pcOwner = Just iid})
    EncounterCard ec -> EncounterCard (ec {ecOwner = Just iid})
    VengeanceCard vc -> VengeanceCard (go vc)

setTaboo :: CardGen m => Maybe TabooList -> Card -> m Card
setTaboo mtaboo card = do
  let result = go card
  replaceCard (toCardId result) result
  pure result
 where
  go = \case
    PlayerCard pc -> PlayerCard (pc {pcTabooList = mtaboo, pcMutated = tabooMutated mtaboo pc})
    other -> other

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  | VengeanceCard Card
  deriving stock (Show, Ord, Data)

instance HasTraits Card where
  toTraits = \case
    PlayerCard pc -> case pc.cardCode of
      "09021" ->
        let customizations = cdCustomizations $ toCardDef pc
         in toTraits pc
              <> if hasCustomization_ customizations (pcCustomizations pc) Enchanted then singleton Relic else mempty
      "09022" ->
        let customizations = cdCustomizations $ toCardDef pc
         in toTraits pc
              <> if hasCustomization_ customizations (pcCustomizations pc) Heirloom then singleton Relic else mempty
      _ -> toTraits pc
    EncounterCard ec -> toTraits ec
    VengeanceCard c -> toTraits c

instance HasField "victoryPoints" Card (Maybe Int) where
  getField = (.victoryPoints) . toCardDef

instance HasField "name" Card Name where
  getField = toName

instance HasField "title" Card Text where
  getField = toTitle

instance HasField "cardCode" Card CardCode where
  getField = toCardCode

instance HasField "keywords" Card (Set Keyword) where
  getField = cdKeywords . toCardDef

instance HasField "isRevelation" Card Bool where
  getField = isRevelation . cdRevelation . toCardDef

instance HasField "mutated" Card (Maybe Text) where
  getField = \case
    PlayerCard pc -> pcMutated pc
    EncounterCard _ -> Nothing
    VengeanceCard _ -> Nothing
  {-# INLINE getField #-}

instance HasField "customizations" Card Customizations where
  getField = \case
    PlayerCard pc -> pcCustomizations pc
    EncounterCard _ -> mempty
    VengeanceCard _ -> mempty
  {-# INLINE getField #-}

instance HasField "id" Card CardId where
  getField = \case
    PlayerCard pc -> pc.id
    EncounterCard ec -> ec.id
    VengeanceCard vc -> vc.id
  {-# INLINE getField #-}

isEncounterCard :: Card -> Bool
isEncounterCard = \case
  PlayerCard _ -> False
  EncounterCard ec -> case cdCardSubType (toCardDef ec) of
    Just Weakness -> False
    _ -> True
  VengeanceCard _ -> False

instance HasField "actions" Card [Action] where
  getField = cdActions . toCardDef

instance HasField "skills" Card [SkillIcon] where
  getField = cdSkills . toCardDef

instance HasField "icons" Card [SkillIcon] where
  getField = cdSkills . toCardDef

instance HasField "cost" Card (Maybe CardCost) where
  getField = cdCost . toCardDef

instance HasField "printedCost" Card Int where
  getField = (.printedCost) . toCardDef

instance HasField "level" Card (Maybe Int) where
  getField = cdLevel . toCardDef

instance HasField "kind" Card CardType where
  getField = cdCardType . toCardDef

instance HasField "owner" Card (Maybe InvestigatorId) where
  getField = toCardOwner

instance HasField "singleSided" Card Bool where
  getField = not . cdDoubleSided . toCardDef

instance Eq Card where
  a == b = toCardId a == toCardId b

flipCard :: Card -> Card
flipCard (EncounterCard ec) =
  if cdDoubleSided (toCardDef ec)
    then case cdOtherSide (toCardDef ec) of
      Just otherSideCode -> EncounterCard $ ec {ecCardCode = otherSideCode, ecOriginalCardCode = otherSideCode}
      Nothing -> EncounterCard $ ec {ecIsFlipped = not <$> ecIsFlipped ec}
    else EncounterCard ec {ecIsFlipped = Just False}
flipCard (PlayerCard pc) = case cdOtherSide (toCardDef pc) of
  Just otherSide -> PlayerCard $ pc {pcCardCode = otherSide}
  Nothing -> PlayerCard pc
flipCard (VengeanceCard c) = VengeanceCard c

_PlayerCard :: Traversal' Card PlayerCard
_PlayerCard f (PlayerCard pc) = PlayerCard <$> f pc
_PlayerCard _ other = pure other

onlyPlayerCards :: [Card] -> [PlayerCard]
onlyPlayerCards = mapMaybe (preview _PlayerCard)

_EncounterCard :: Traversal' Card EncounterCard
_EncounterCard f (EncounterCard pc) = EncounterCard <$> f pc
_EncounterCard _ other = pure other

onlyEncounterCards :: [Card] -> [EncounterCard]
onlyEncounterCards = mapMaybe (preview _EncounterCard)

newtype RevealedCard = RevealedCard Card

instance Named RevealedCard where
  toName (RevealedCard card) =
    let def = toCardDef card
     in fromMaybe (cdName def) (cdRevealedName def)

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

instance HasCost Card where
  getCost (PlayerCard card) = getCost card
  getCost (EncounterCard _) = 0
  getCost (VengeanceCard _) = 0

isDynamic :: Card -> Bool
isDynamic (PlayerCard card) = case cdCost (toCardDef card) of
  Just DynamicCost -> True
  Just (MaxDynamicCost _) -> True
  _ -> False
isDynamic (EncounterCard _) = False
isDynamic (VengeanceCard _) = False

maxDynamic :: Card -> Maybe GameCalculation
maxDynamic (PlayerCard card) = case cdCost (toCardDef card) of
  Just DynamicCost -> Nothing
  Just (MaxDynamicCost x) -> Just x
  _ -> Nothing
maxDynamic (EncounterCard _) = Nothing
maxDynamic (VengeanceCard _) = Nothing

isFastCard :: Card -> Bool
isFastCard (PlayerCard card) =
  let CardDef {..} = toCardDef card in isJust cdFastWindow
isFastCard (EncounterCard _) = False
isFastCard (VengeanceCard _) = False

toPlayerCard :: IsCard c => c -> Maybe PlayerCard
toPlayerCard c = case toCard c of
  PlayerCard pc -> Just pc
  EncounterCard {} -> Nothing
  VengeanceCard inner -> toPlayerCard inner

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

$(deriveJSON defaultOptions ''Card)

-- Not in Arkham.Matcher to avoid circular imports
class IsThisCard a where
  isThisCard :: IsCard b => b -> a

instance IsThisCard CardMatcher where
  isThisCard = CardWithId . toCardId

instance IsThisCard ExtendedCardMatcher where
  isThisCard = basic . isThisCard
