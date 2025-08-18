module Arkham.Decklist (module Arkham.Decklist, module Arkham.Decklist.Type) where

import Arkham.Prelude hiding (try)

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Customization
import Arkham.Decklist.Type
import Arkham.Id
import Arkham.Investigator
import Arkham.Name
import Arkham.PlayerCard
import Arkham.SkillType
import Arkham.Taboo.Types
import Arkham.Trait
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import GHC.Records
import Text.Parsec (
  ParsecT,
  alphaNum,
  char,
  digit,
  many1,
  optionMaybe,
  parse,
  sepBy,
  sepBy1,
  space,
  string,
  try,
  unexpected,
 )
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

decklistInvestigatorId :: ArkhamDBDecklist -> InvestigatorId
decklistInvestigatorId decklist = fromMaybe (investigator_code decklist) do
  meta' <- meta decklist
  ArkhamDBDecklistMeta {alternate_front} <- decode (encodeUtf8 $ fromStrict meta')
  guard (alternate_front /= Just "") *> alternate_front

loadDecklistCards
  :: CardGen m => (ArkhamDBDecklist -> Map CardCode Int) -> ArkhamDBDecklist -> m [PlayerCard]
loadDecklistCards f decklist = do
  results <- for (Map.toList $ f decklist) $ \(cardCode, count') ->
    replicateM
      count'
      ( genPlayerCardWith (lookupPlayerCardDef cardCode)
          $ applyCustomizations decklist
          . setPlayerCardOwner (normalizeInvestigatorId $ decklistInvestigatorId decklist)
          . Arkham.Card.PlayerCard.setTaboo (fromTabooId $ taboo_id decklist)
      )
  pure $ fold results

loadExtraDeck :: CardGen m => ArkhamDBDecklist -> m [PlayerCard]
loadExtraDeck decklist = case meta decklist of
  Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
    Just (Object o) ->
      case KeyMap.lookup "extra_deck" o of
        Just (String s) ->
          let codes = T.splitOn "," s
              convert =
                applyCustomizations decklist
                  . setPlayerCardOwner (normalizeInvestigatorId $ decklistInvestigatorId decklist)
           in traverse ((`genPlayerCardWith` convert) . lookupPlayerCardDef . CardCode) codes
        _ -> loadDecklistCards sideSlots decklist
    _ -> loadDecklistCards sideSlots decklist
  _ -> loadDecklistCards sideSlots decklist

data ArkhamDBDecklistMeta = ArkhamDBDecklistMeta
  { alternate_front :: Maybe InvestigatorId
  , attachments_11080 :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

-- things we can choose: cards, traits, skills
applyCustomizations :: ArkhamDBDecklist -> PlayerCard -> PlayerCard
applyCustomizations deckList pCard = case meta deckList of
  Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
    Just (Object o) ->
      case KeyMap.lookup (fromText $ "cus_" <> unCardCode (pcCardCode pCard)) o of
        Just (fromJSON -> Success customizations) -> case parse parseCustomizations "" customizations of
          Left _ -> pCard
          Right cs -> pCard {pcCustomizations = cs}
        _ -> pCard
    _ -> pCard
  _ -> pCard

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
      <$> ( (string "willpower" $> SkillWillpower)
              <|> (string "intellect" $> SkillIntellect)
              <|> (string "combat" $> SkillCombat)
              <|> (string "agility" $> SkillAgility)
          )
  parseTraits = sepBy1 parseTrait (char '^')
  parseTrait = do
    t <- many1 (alphaNum <|> space)
    case fromJSON @Trait (String . T.concat . T.words . T.toTitle $ pack t) of
      Success x -> pure $ ChosenTrait x
      _ -> unexpected ("invalid trait: " ++ t)

decklistAttachments :: ArkhamDBDecklist -> Map CardCode [CardCode]
decklistAttachments decklist = case meta decklist of
  Nothing -> mempty
  Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
    Nothing -> mempty
    Just ArkhamDBDecklistMeta {attachments_11080} ->
      case attachments_11080 of
        Nothing -> mempty
        Just s ->
          let codes = T.splitOn "," s
           in Map.fromList [(CardCode "11080", map CardCode codes)]
