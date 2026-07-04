module Arkham.Decklist.RandomBasicWeakness where

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.ClassSymbol
import Arkham.Decklist.Type
import Arkham.PlayerCard
import Arkham.Prelude
import Arkham.Taboo
import Data.Text qualified as T

newtype ArkhamBuildCardPool = ArkhamBuildCardPool [Text]
  deriving stock (Show, Eq, Ord)

data RandomBasicWeaknessContext = RandomBasicWeaknessContext
  { rbwInvestigatorClass :: ClassSymbol
  , rbwPlayerCount :: Int
  , rbwDecklist :: Maybe ArkhamDBDecklist
  , rbwStandalone :: Bool
  }
  deriving stock (Show, Eq, Ord)

newtype ArkhamBuildDecklistMeta = ArkhamBuildDecklistMeta
  { card_pool :: Maybe Text
  }
  deriving stock (Show, Eq)

instance FromJSON ArkhamBuildDecklistMeta where
  parseJSON = withObject "ArkhamBuildDecklistMeta" $ \o -> do
    card_pool <- o .:? "card_pool"
    pure $ ArkhamBuildDecklistMeta {..}

parseArkhamBuildCardPool :: ArkhamDBDecklist -> Maybe ArkhamBuildCardPool
parseArkhamBuildCardPool decklist = do
  metaText <- meta decklist
  ArkhamBuildDecklistMeta {card_pool} <- decode (encodeUtf8 $ fromStrict metaText)
  poolText <- card_pool
  let tokens = filter (not . T.null) $ T.strip <$> T.splitOn "," poolText
  guard (notNull tokens)
  pure $ ArkhamBuildCardPool tokens

randomBasicWeaknessCandidates :: RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessCandidates ctx =
  filter (weaknessFilter ctx) $ tabooMutate ctx <$> allBasicWeaknesses

randomBasicWeaknessSamplingCandidates :: RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessSamplingCandidates ctx =
  let candidates = randomBasicWeaknessCandidates ctx
   in if null candidates then randomBasicWeaknessCandidatesIgnoringCardPool ctx else candidates

sampleRandomBasicWeakness :: MonadRandom m => RandomBasicWeaknessContext -> m CardDef
sampleRandomBasicWeakness ctx =
  sample
    $ fromJustNote "No random basic weakness candidates"
    $ nonEmpty
    $ randomBasicWeaknessSamplingCandidates ctx

tabooMutate :: RandomBasicWeaknessContext -> CardDef -> CardDef
tabooMutate RandomBasicWeaknessContext {rbwDecklist} cardDef =
  maybe id tabooListModify (rbwDecklist >>= fromTabooId . taboo_id) cardDef

randomBasicWeaknessCandidatesIgnoringCardPool :: RandomBasicWeaknessContext -> [CardDef]
randomBasicWeaknessCandidatesIgnoringCardPool ctx =
  filter (weaknessFilterIgnoringCardPool ctx) $ tabooMutate ctx <$> allBasicWeaknesses

weaknessFilter :: RandomBasicWeaknessContext -> CardDef -> Bool
weaknessFilter ctx cardDef =
  and
    [ multiplayerAllowed ctx cardDef
    , classAllowed ctx cardDef
    , standaloneAllowed ctx cardDef
    , cardPoolAllowed ctx cardDef
    ]

weaknessFilterIgnoringCardPool :: RandomBasicWeaknessContext -> CardDef -> Bool
weaknessFilterIgnoringCardPool ctx cardDef =
  and
    [ multiplayerAllowed ctx cardDef
    , classAllowed ctx cardDef
    , standaloneAllowed ctx cardDef
    ]

multiplayerAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
multiplayerAllowed RandomBasicWeaknessContext {rbwPlayerCount} cardDef =
  rbwPlayerCount >= 2 || MultiplayerOnly `notElem` cdDeckRestrictions cardDef

classAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
classAllowed RandomBasicWeaknessContext {rbwInvestigatorClass} cardDef =
  all restrictionAllowsClass $ cdDeckRestrictions cardDef
 where
  restrictionAllowsClass = \case
    OnlyClass c -> c == rbwInvestigatorClass
    _ -> True

standaloneAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
standaloneAllowed RandomBasicWeaknessContext {rbwStandalone} cardDef =
  not rbwStandalone || CampaignModeOnly `notElem` cdDeckRestrictions cardDef

cardPoolAllowed :: RandomBasicWeaknessContext -> CardDef -> Bool
cardPoolAllowed RandomBasicWeaknessContext {rbwDecklist} cardDef =
  case rbwDecklist >>= parseArkhamBuildCardPool of
    Nothing -> True
    Just (ArkhamBuildCardPool []) -> True
    Just (ArkhamBuildCardPool tokens) ->
      let predicates = mapMaybe tokenPredicate tokens
       in null predicates || any ($ toCardCode cardDef) predicates

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
