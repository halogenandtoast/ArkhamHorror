module Arkham.Decklist (module Arkham.Decklist, module Arkham.Decklist.Type) where

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Customization
import Arkham.Decklist.Type
import Arkham.Id
import Arkham.Investigator
import Arkham.Name
import Arkham.PlayerCard
import Arkham.Prelude hiding (try, (<|>))
import Arkham.Taboo.Types
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Records
import Text.Parsec
import Text.Read (read)

data Decklist = Decklist
  { decklistInvestigator :: InvestigatorId
  , decklistCards :: [PlayerCard]
  , decklistExtraDeck :: [PlayerCard]
  , decklistTaboo :: Maybe TabooList
  , decklistUrl :: Maybe Text
  , decklistCardAttachments :: Map CardCode [CardCode]
  }
  deriving stock Show

instance HasField "investigator" Decklist InvestigatorId where
  getField = decklistInvestigator

instance HasField "cards" Decklist [PlayerCard] where
  getField = decklistCards

instance HasField "extra" Decklist [PlayerCard] where
  getField = decklistExtraDeck

instance HasField "taboo" Decklist (Maybe TabooList) where
  getField = decklistTaboo

instance HasField "url" Decklist (Maybe Text) where
  getField = decklistUrl

instance HasField "attachments" Decklist (Map CardCode [CardCode]) where
  getField = decklistCardAttachments

type Parser = ParsecT Text () Identity

loadDecklist :: CardGen m => ArkhamDBDecklist -> m Decklist
loadDecklist decklist =
  Decklist (decklistInvestigatorId decklist)
    <$> loadDecklistCards slots decklist
    <*> loadExtraDeck decklist
    <*> pure (fromTabooId $ taboo_id decklist)
    <*> pure (url decklist)
    <*> pure (decklistAttachments decklist)

loadDecklistCards
  :: CardGen m => (ArkhamDBDecklist -> Map CardCode Int) -> ArkhamDBDecklist -> m [PlayerCard]
loadDecklistCards f decklist =
  fold <$> for (Map.toList $ f decklist) \(cardCode, count') ->
    replicateM count' do
      genPlayerCardWith (lookupPlayerCardDef cardCode)
        $ applyCustomizations decklist
        . setPlayerCardOwner (normalizeInvestigatorId $ decklistInvestigatorId decklist)
        . Arkham.Card.PlayerCard.setTaboo (fromTabooId $ taboo_id decklist)

loadExtraDeck :: CardGen m => ArkhamDBDecklist -> m [PlayerCard]
loadExtraDeck decklist = do
  let
    mResult = do
      meta' <- meta decklist
      Object o <- decode (encodeUtf8 $ fromStrict meta')
      String s <- KeyMap.lookup "extra_deck" o
      pure $ T.splitOn "," s

  case mResult of
    Nothing -> loadDecklistCards sideSlots decklist
    Just codes -> do
      let convert =
            applyCustomizations decklist
              . setPlayerCardOwner (normalizeInvestigatorId $ decklistInvestigatorId decklist)
      traverse ((`genPlayerCardWith` convert) . lookupPlayerCardDef . CardCode) codes

-- things we can choose: cards, traits, skills
applyCustomizations :: ArkhamDBDecklist -> PlayerCard -> PlayerCard
applyCustomizations deckList pCard = fromMaybe pCard do
  meta' <- meta deckList
  Object o <- decode (encodeUtf8 $ fromStrict meta')
  s <- KeyMap.lookup (fromText $ "cus_" <> unCardCode (pcCardCode pCard)) o
  case fromJSON s of
    Success (customizations :: Text) -> do
      cs <- parseMaybe parseCustomizations "" customizations
      pure $ pCard {pcCustomizations = cs}
    _ -> Nothing

parseMaybe :: Parser a -> SourceName -> Text -> Maybe a
parseMaybe p sName input = either (const Nothing) Just $ parse p sName input

parseCustomizations :: Parser Customizations
parseCustomizations = IntMap.fromList <$> sepBy parseEntry (char ',')
 where
  parseEntry = (,) <$> parseInt <*> (char '|' *> parseCustomization)
  parseInt = read <$> many1 digit
  parseCardCodes =
    sepBy1
      ( maybe (unexpected "invalid card code") (pure . ChosenCard . toTitle)
          . lookupCardDef
          . CardCode
          . pack
          =<< many1 alphaNum
      )
      (char '^')
  parseCustomization = do
    n <- parseInt
    choices <-
      optionMaybe
        $ char '|'
        *> (try parseSkillTypes <|> try parseTraits <|> try parseIndex <|> try parseCardCodes <|> pure [])
    pure (n, fromMaybe [] choices)
  parseIndex = do
    n <- parseInt
    if n <= 4
      then pure . pure $ ChosenIndex n
      else unexpected "index must be between 1 and 4"
  parseSkillTypes = sepBy1 parseSkillType (char '^')
  parseSkillType =
    ChosenSkill
      <$> ( (string "willpower" $> #willpower)
              <|> (string "intellect" $> #intellect)
              <|> (string "combat" $> #combat)
              <|> (string "agility" $> #agility)
          )
  parseTraits = sepBy1 parseTrait (char '^')
  parseTrait = do
    t <- many1 (alphaNum <|> space)
    case fromJSON (String . T.concat . T.words . T.toTitle $ pack t) of
      Success x -> pure $ ChosenTrait x
      _ -> unexpected ("invalid trait: " ++ t)

decklistAttachments :: ArkhamDBDecklist -> Map CardCode [CardCode]
decklistAttachments decklist = fromMaybe mempty do
  meta' <- meta decklist
  ArkhamDBDecklistMeta {attachments_11080} <- decode (encodeUtf8 $ fromStrict meta')
  codes <- T.splitOn "," <$> attachments_11080
  pure $ Map.fromList [(CardCode "11080", map CardCode codes)]
