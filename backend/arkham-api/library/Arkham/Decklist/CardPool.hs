module Arkham.Decklist.CardPool where

import Arkham.Card.CardCode
import Arkham.Prelude
import Data.Char (isDigit)
import Data.Text qualified as T

newtype ArkhamBuildCardPool = ArkhamBuildCardPool [Text]
  deriving stock (Show, Eq, Ord, Data)
  deriving newtype (ToJSON, FromJSON)

{- | Whether a card is legal for the pool the player chose while building the deck. No
pool, an empty pool, or a pool whose tokens we don't recognize allows everything.
-}
cardPoolAllows :: Maybe ArkhamBuildCardPool -> CardCode -> Bool
cardPoolAllows mPool cardCode = case mPool of
  Nothing -> True
  Just (ArkhamBuildCardPool []) -> True
  Just (ArkhamBuildCardPool tokens) ->
    let predicates = mapMaybe tokenPredicate tokens
     in null predicates || any ($ cardCode) predicates

tokenPredicate :: Text -> Maybe (CardCode -> Bool)
tokenPredicate token
  | token == "cycle:investigator_decks_ch2" =
      Just \cardCode -> cardCode.isChapterTwo && scopeAllows investigatorDecks cardCode
  | Just packToken <- T.stripPrefix "pack:" token =
      Just
        if T.null packToken
          then const False
          else anyScope $ fromMaybe [CodePrefix packToken] $ tokenScopes packToken
  | otherwise = anyScope <$> tokenScopes token

{- | What a pool token admits. Every product is numbered in a contiguous block, so a
range of card numbers says exactly which cards a token covers. 'CodePrefix' is the
fallback for a @pack:@ token we have no range for, and is matched literally.
-}
data CardPoolScope = CardNumbers Int Int | CodePrefix Text

scopeAllows :: CardPoolScope -> CardCode -> Bool
scopeAllows = \case
  CodePrefix prefix -> T.isPrefixOf prefix . unCardCode
  CardNumbers low high -> maybe False (\n -> n >= low && n <= high) . cardNumber

anyScope :: [CardPoolScope] -> CardCode -> Bool
anyScope scopes cardCode = any (`scopeAllows` cardCode) scopes

{- | The number a card code carries, ignoring any side suffix (@03276a@ is 3276). A
homebrew or custom code is not numbered at all, and belongs to no printed product.
-}
cardNumber :: CardCode -> Maybe Int
cardNumber = readMay . T.takeWhile isDigit . unCardCode

-- | A cycle's own numbering block: @02000@-@02999@ for The Dunwich Legacy, and so on.
cycleCards :: Int -> CardPoolScope
cycleCards cycleNumber = CardNumbers (cycleNumber * 1000) (cycleNumber * 1000 + 999)

investigatorDecks :: CardPoolScope
investigatorDecks = CardNumbers 60000 60999

{- | The half of a @60Xyy@ block one investigator starter deck owns. Two decks share
each block: the Chapter 1 deck is numbered @60X01@-@60X49@ and the Chapter 2 deck that
reuses the block @60X51@-@60X99@ -- the same split
'Arkham.Card.CardCode.chapterTwoPrefixes' draws.
-}
chapterOneDeck, chapterTwoDeck :: Int -> CardPoolScope
chapterOneDeck cycleNumber = CardNumbers (starterDeckBlock cycleNumber) (starterDeckBlock cycleNumber + 49)
chapterTwoDeck cycleNumber = CardNumbers (starterDeckBlock cycleNumber + 50) (starterDeckBlock cycleNumber + 99)

starterDeckBlock :: Int -> Int
starterDeckBlock cycleNumber = 60000 + cycleNumber * 100

tokenScopes :: Text -> Maybe [CardPoolScope]
tokenScopes token = case fromMaybe token $ T.stripPrefix "cycle:" token of
  "core" -> Just [CardNumbers 1000 1199]
  "rcore" -> Just [CardNumbers 1500 1699]
  "dwl" -> Just [cycleCards 2]
  "dwlp" -> Just [cycleCards 2]
  "ptc" -> Just [cycleCards 3]
  "ptcp" -> Just [cycleCards 3]
  "tfa" -> Just [cycleCards 4]
  "tfap" -> Just [cycleCards 4]
  "tcu" -> Just [cycleCards 5]
  "tcup" -> Just [cycleCards 5]
  "tde" -> Just [cycleCards 6]
  "tdep" -> Just [cycleCards 6]
  "tic" -> Just [cycleCards 7]
  "ticp" -> Just [cycleCards 7]
  "eote" -> Just [cycleCards 8]
  "eoep" -> Just [cycleCards 8]
  "tsk" -> Just [cycleCards 9]
  "tskp" -> Just [cycleCards 9]
  "fhv" -> Just [cycleCards 10]
  "fhvp" -> Just [cycleCards 10]
  "tdc" -> Just [cycleCards 11]
  "tdcp" -> Just [cycleCards 11]
  "core_ch2" -> Just [cycleCards 12]
  "core2026" -> Just [cycleCards 12]
  "core_2026" -> Just [cycleCards 12]
  "return" -> Just [CardNumbers 50000 59999]
  "rtnotz" -> Just [cycleCards 50]
  "rtdwl" -> Just [cycleCards 51]
  "rtptc" -> Just [cycleCards 52]
  "rttfa" -> Just [cycleCards 53]
  "rttcu" -> Just [cycleCards 54]
  "investigator_decks" -> Just [investigatorDecks]
  "nat" -> Just [chapterOneDeck 1]
  "tom" -> Just [chapterTwoDeck 1]
  "har" -> Just [chapterOneDeck 2]
  "car" -> Just [chapterTwoDeck 2]
  "win" -> Just [chapterOneDeck 3]
  "and" -> Just [chapterTwoDeck 3]
  "jac" -> Just [chapterOneDeck 4]
  "mar" -> Just [chapterTwoDeck 4]
  "ste" -> Just [chapterOneDeck 5]
  "mig" -> Just [chapterTwoDeck 5]
  _ -> Nothing
