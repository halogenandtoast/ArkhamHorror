module Arkham.Decklist (module Arkham.Decklist, module Arkham.Decklist.Type) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.PlayerCard
import Arkham.Decklist.Type
import Arkham.Id
import Arkham.Investigator
import Arkham.PlayerCard
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.IntMap qualified as IntMap
import Data.Map.Strict qualified as Map
import Text.Parsec (ParsecT, char, digit, many1, parse, sepBy)
import Text.Read (read)

type Parser = ParsecT Text () Identity

loadDecklist :: CardGen m => ArkhamDBDecklist -> m (InvestigatorId, [PlayerCard])
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

loadDecklistCards :: CardGen m => ArkhamDBDecklist -> m [PlayerCard]
loadDecklistCards decklist = do
  results <- forM (Map.toList $ slots decklist) $ \(cardCode, count') ->
    replicateM
      count'
      ( genPlayerCardWith (lookupPlayerCardDef cardCode)
          $ applyCustomizations decklist
          . setPlayerCardOwner (normalizeInvestigatorId $ investigator_code decklist)
      )
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
