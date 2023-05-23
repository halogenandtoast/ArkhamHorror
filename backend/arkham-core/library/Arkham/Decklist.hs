module Arkham.Decklist where

import Arkham.Prelude

import Arkham.Card
import Arkham.Id
import Arkham.PlayerCard
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Text.Parsec (ParsecT, char, digit, many1, parse, sepBy)
import Text.Read (read)

type Parser = ParsecT Text () Identity

data ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: Map CardCode Int
  , investigator_code :: InvestigatorId
  , investigator_name :: Text
  , meta :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

loadDecklist :: (CardGen m) => ArkhamDBDecklist -> m (InvestigatorId, [PlayerCard])
loadDecklist decklist = (investigatorId,) <$> loadDecklistCards decklist
 where
  investigatorId = case meta decklist of
    Nothing -> investigator_code decklist
    Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
      Nothing -> investigator_code decklist
      Just ArkhamDBDecklistMeta {..} ->
        if alternate_front == ""
          then investigator_code decklist
          else alternate_front

decklistInvestigatorId :: ArkhamDBDecklist -> InvestigatorId
decklistInvestigatorId decklist = case meta decklist of
  Nothing -> investigator_code decklist
  Just meta' -> case decode (encodeUtf8 $ fromStrict meta') of
    Nothing -> investigator_code decklist
    Just ArkhamDBDecklistMeta {..} ->
      if alternate_front == ""
        then investigator_code decklist
        else alternate_front

loadDecklistCards :: (CardGen m) => ArkhamDBDecklist -> m [PlayerCard]
loadDecklistCards decklist = do
  results <- forM (Map.toList $ slots decklist) $ \(cardCode, count') ->
    replicateM count' (applyCustomizations decklist <$> genPlayerCard (lookupPlayerCardDef cardCode))
  pure $ fold results

newtype ArkhamDBDecklistMeta = ArkhamDBDecklistMeta
  { alternate_front :: InvestigatorId
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

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
 where
  parseCustomizations :: Parser (IntMap Int)
  parseCustomizations = do
    let parseInt = read <$> many1 digit
    IntMap.fromList <$> sepBy ((,) <$> parseInt <*> (char '|' *> parseInt)) (char ',')
