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
import Arkham.Trait
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
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

type Parser = ParsecT Text () Identity

loadDecklist :: CardGen m => ArkhamDBDecklist -> m (InvestigatorId, [PlayerCard])
loadDecklist decklist = (decklistInvestigatorId decklist,) <$> loadDecklistCards decklist

decklistInvestigatorId :: ArkhamDBDecklist -> InvestigatorId
decklistInvestigatorId decklist = case meta decklist of
  Nothing -> investigator_code decklist
  Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
    Nothing -> investigator_code decklist
    Just ArkhamDBDecklistMeta {..} ->
      if alternate_front == ""
        then investigator_code decklist
        else alternate_front

loadDecklistCards :: CardGen m => ArkhamDBDecklist -> m [PlayerCard]
loadDecklistCards decklist = do
  results <- for (Map.toList $ slots decklist) $ \(cardCode, count') ->
    replicateM
      count'
      ( genPlayerCardWith (lookupPlayerCardDef cardCode)
          $ applyCustomizations decklist
          . setPlayerCardOwner (normalizeInvestigatorId $ decklistInvestigatorId decklist)
      )
  pure $ fold results

newtype ArkhamDBDecklistMeta = ArkhamDBDecklistMeta
  { alternate_front :: InvestigatorId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

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
  parseIndex = pure . ChosenIndex <$> parseInt
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
