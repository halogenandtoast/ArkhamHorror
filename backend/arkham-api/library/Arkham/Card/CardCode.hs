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

flippedCardCode :: CardCode -> CardCode
flippedCardCode (CardCode a) = CardCode (a <> "b")

instance HasField "flipped" CardCode CardCode where
  getField = flippedCardCode

-- We special case the stranger since ADB calls them a b c
instance Eq CardCode where
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

newtype CardCodeExact = CardCodeExact {unCardCodeExact :: CardCode}

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

exactCardCode :: HasCardCode a => a -> CardCodeExact
exactCardCode = CardCodeExact . toCardCode

class HasCardCode a where
  toCardCode :: a -> CardCode

instance HasCardCode CardCode where
  toCardCode = id
