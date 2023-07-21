module Arkham.Card.CardCode where

import Arkham.Prelude
import Data.Aeson.Types
import Data.Text qualified as T

newtype CardCode = CardCode {unCardCode :: Text}
  deriving newtype (Show, Ord, Hashable, IsString)

instance Eq CardCode where
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

instance ToJSON CardCode where
  toJSON = toJSON . T.cons 'c' . unCardCode

instance FromJSON CardCode where
  parseJSON =
    withText "CardCode" $ \t -> pure $ CardCode $ T.dropWhile (== 'c') t

instance ToJSONKey CardCode where
  toJSONKey = toJSONKeyText (T.cons 'c' . unCardCode)

instance FromJSONKey CardCode where
  fromJSONKey = CardCode . T.dropWhile (== 'c') <$> fromJSONKey

class HasCardCode a where
  toCardCode :: a -> CardCode

instance HasCardCode CardCode where
  toCardCode = id
