{-# LANGUAGE OverloadedStrings #-}

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

sampleRandomBasicWeakness :: MonadRandom m => RandomBasicWeaknessContext -> m CardDef
sampleRandomBasicWeakness ctx =
  sample
    $ fromJustNote "No random basic weakness candidates"
    $ nonEmpty
    $ randomBasicWeaknessCandidates ctx

tabooMutate :: RandomBasicWeaknessContext -> CardDef -> CardDef
tabooMutate RandomBasicWeaknessContext {rbwDecklist} cardDef =
  maybe id tabooListModify (rbwDecklist >>= fromTabooId . taboo_id) cardDef

weaknessFilter :: RandomBasicWeaknessContext -> CardDef -> Bool
weaknessFilter ctx cardDef =
  and
    [ multiplayerAllowed ctx cardDef
    , classAllowed ctx cardDef
    , standaloneAllowed ctx cardDef
    , cardPoolAllowed ctx cardDef
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
      let prefix = T.drop 5 token
       in Just $ if T.null prefix then const False else cardCodeStartsWith prefix
  | otherwise = cardCodeStartsWith <$> cycleTokenPrefix token

cardCodeStartsWith :: Text -> CardCode -> Bool
cardCodeStartsWith prefix = T.isPrefixOf prefix . unCardCode

cycleTokenPrefix :: Text -> Maybe Text
cycleTokenPrefix = \case
  "cycle:core" -> Just "01"
  "cycle:dwl" -> Just "02"
  "cycle:ptc" -> Just "03"
  "cycle:tfa" -> Just "04"
  "cycle:tcu" -> Just "05"
  "cycle:tde" -> Just "06"
  "cycle:tic" -> Just "07"
  "cycle:eote" -> Just "08"
  "cycle:tsk" -> Just "09"
  "cycle:fhv" -> Just "10"
  "cycle:tdc" -> Just "11"
  "cycle:core_ch2" -> Just "12"
  "cycle:return" -> Just "5"
  "cycle:investigator_decks" -> Just "60"
  _ -> Nothing
