module Arkham.Card.CardCode where

import Arkham.Prelude
import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Records

newtype CardCode = CardCode {unCardCode :: Text}
  deriving stock Data
  deriving newtype (Show, Ord, Read, Hashable, IsString)

-- these card codes get a `b` added after the normal designator
exceptionCardCodes :: [Text]
exceptionCardCodes = ["03047a", "03047b", "03047c", "03279a", "03279b"]

-- these card codes end in a/b but are two *distinct printings*, not two sides of
-- one card, so they must compare exactly. Written in Rock's rail tunnels are
-- separate cards (or the two quantity-2 copies, which carry different rail
-- icons); letting them cross-match makes set-aside lookups grab the wrong copy.
distinctPrintingCardCodes :: [Text]
distinctPrintingCardCodes =
  [ "10510a"
  , "10510b"
  , "10511a"
  , "10511b"
  , "10512a"
  , "10512b"
  , "10513a"
  , "10513b"
  , "10514a"
  , "10514b"
  ]

flippedCardCode :: CardCode -> CardCode
flippedCardCode (CardCode a) = case T.unsnoc a of
  Just (base, 'b') -> CardCode base
  _ -> CardCode (a <> "b")

instance HasField "flipped" CardCode CardCode where
  getField = flippedCardCode

-- We special case the stranger since ADB calls them a b c
instance Eq CardCode where
  (CardCode a) == (CardCode b)
    | a `elem` distinctPrintingCardCodes || b `elem` distinctPrintingCardCodes = a == b
  (CardCode a) == (CardCode b)
    | a `elem` exceptionCardCodes || b `elem` exceptionCardCodes =
        a == b || (a <> "b") == b || a == (b <> "b")
  (CardCode a) == (CardCode b) =
    a == b || (toBase a == toBase b && complements (sideOf a) (sideOf b))
   where
    sideSuffixes = "abcd" :: [Char]
    isSideSuffix = (`elem` sideSuffixes)
    toBase = T.dropWhileEnd isSideSuffix
    sideOf = listToMaybe . T.unpack . T.takeWhileEnd isSideSuffix
    complements (Just x) (Just y) = case x of
      'a' -> y == 'b'
      'b' -> y == 'a'
      'c' -> y == 'd'
      'd' -> y == 'c'
      n -> error $ n : " is not a valid side"
    complements _ _ = False

cardCodeExactEq :: CardCode -> CardCode -> Bool
cardCodeExactEq (CardCode a) (CardCode b) = a == b

newtype CardCodeExact = CardCodeExact CardCode
  deriving newtype (Show, IsString, FromJSON, ToJSON, Ord)
  deriving stock Data

instance Eq CardCodeExact where
  (CardCodeExact a) == (CardCodeExact b) = cardCodeExactEq a b

instance ToJSON CardCode where
  toJSON = toJSON . T.cons 'c' . unCardCode

instance FromJSON CardCode where
  parseJSON =
    withText "CardCode" $ \t -> pure $ CardCode $ T.dropWhile (== 'c') t

instance ToJSONKey CardCode where
  toJSONKey = toJSONKeyText (T.cons 'c' . unCardCode)

instance FromJSONKey CardCode where
  fromJSONKey = CardCode . T.dropWhile (== 'c') <$> fromJSONKey

isCardCode :: (HasCardCode a, HasCardCode b) => a -> b -> Bool
isCardCode a b = toCardCode a == toCardCode b

exactCardCode :: HasCardCode a => a -> CardCodeExact
exactCardCode = CardCodeExact . toCardCode

-- | Card-code prefixes belonging to Chapter 2.
--
-- Includes the Core 2026 cycle (@12xxx@) and the upper half (@60X5y@–@60X9y@,
-- for X in 1..5) of each standalone investigator deck cycle, which is shared
-- with the original Chapter 1 standalone decks but reserved for the Chapter 2
-- packs (Tommy Muldoon (2), Carolyn Fern (2), Andre Patel, Marie Lambeau (2),
-- Miguel De La Cruz).
chapterTwoPrefixes :: [Text]
chapterTwoPrefixes =
  "12" : [T.pack ['6', '0', cycleD, tensD] | cycleD <- "12345", tensD <- "56789"]

isChapterTwo :: CardCode -> Bool
isChapterTwo (CardCode code) = any (`T.isPrefixOf` code) chapterTwoPrefixes

instance HasField "isChapterTwo" CardCode Bool where
  getField = isChapterTwo

class HasCardCode a where
  toCardCode :: a -> CardCode

instance HasCardCode CardCode where
  toCardCode = id
