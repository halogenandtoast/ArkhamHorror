module Arkham.Decklist.CardPool where

import Arkham.Card.CardCode
import Arkham.Prelude
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
      Just \cardCode -> cardCode.isChapterTwo && cardCodeStartsWith "60" cardCode
  | "pack:" `T.isPrefixOf` token =
      let packToken = T.drop 5 token
       in Just
            if T.null packToken
              then const False
              else cardCodeStartsWithAny $ fromMaybe [packToken] $ tokenPrefixes packToken
  | otherwise = cardCodeStartsWithAny <$> tokenPrefixes token

cardCodeStartsWith :: Text -> CardCode -> Bool
cardCodeStartsWith prefix = T.isPrefixOf prefix . unCardCode

cardCodeStartsWithAny :: [Text] -> CardCode -> Bool
cardCodeStartsWithAny prefixes cardCode = any (`cardCodeStartsWith` cardCode) prefixes

tokenPrefixes :: Text -> Maybe [Text]
tokenPrefixes token = case fromMaybe token $ T.stripPrefix "cycle:" token of
  "core" -> Just ["010", "011"]
  "rcore" -> Just ["015", "016"]
  "dwl" -> Just ["02"]
  "dwlp" -> Just ["02"]
  "ptc" -> Just ["03"]
  "ptcp" -> Just ["03"]
  "tfa" -> Just ["04"]
  "tfap" -> Just ["04"]
  "tcu" -> Just ["05"]
  "tcup" -> Just ["05"]
  "tde" -> Just ["06"]
  "tdep" -> Just ["06"]
  "tic" -> Just ["07"]
  "ticp" -> Just ["07"]
  "eote" -> Just ["08"]
  "eoep" -> Just ["08"]
  "tsk" -> Just ["09"]
  "tskp" -> Just ["09"]
  "fhv" -> Just ["10"]
  "fhvp" -> Just ["10"]
  "tdc" -> Just ["11"]
  "tdcp" -> Just ["11"]
  "core_ch2" -> Just ["12"]
  "core2026" -> Just ["12"]
  "core_2026" -> Just ["12"]
  "return" -> Just ["5"]
  "rtnotz" -> Just ["50"]
  "rtdwl" -> Just ["51"]
  "rtptc" -> Just ["52"]
  "rttfa" -> Just ["53"]
  "rttcu" -> Just ["54"]
  "investigator_decks" -> Just ["60"]
  "nat" -> Just ["6010"]
  "tom" -> Just ["6015"]
  "har" -> Just ["6020"]
  "car" -> Just ["6025"]
  "win" -> Just ["6030"]
  "and" -> Just ["6035"]
  "jac" -> Just ["6040"]
  "mar" -> Just ["6045"]
  "ste" -> Just ["6050"]
  "mig" -> Just ["6055"]
  _ -> Nothing
