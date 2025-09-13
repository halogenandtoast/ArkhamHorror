module Arkham.Decklist.Type where

import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Investigator.Cards
import Arkham.Name
import Arkham.Prelude
import GHC.Records

data ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: Map CardCode Int
  , sideSlots :: Map CardCode Int
  , investigator_code :: InvestigatorId
  , investigator_name :: Text
  , meta :: Maybe Text
  , taboo_id :: Maybe Int
  , url :: Maybe Text
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass ToJSON

data ArkhamDBDecklistMeta = ArkhamDBDecklistMeta
  { alternate_front :: Maybe InvestigatorId
  , attachments_11080 :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass FromJSON

decklistInvestigatorId :: ArkhamDBDecklist -> InvestigatorId
decklistInvestigatorId decklist = fromMaybe (investigator_code decklist) do
  meta' <- meta decklist
  ArkhamDBDecklistMeta {alternate_front} <- decode (encodeUtf8 $ fromStrict meta')
  guard (alternate_front /= Just "") *> alternate_front

instance HasField "investigator" ArkhamDBDecklist InvestigatorId where
  getField = decklistInvestigatorId

instance FromJSON ArkhamDBDecklist where
  parseJSON = withObject "ArkhamDBDecklist" $ \o -> do
    slots <- o .: "slots"
    sideSlots <- o .: "sideSlots" <|> pure mempty
    investigator_code <- o .: "investigator_code"
    investigator_name <-
      o
        .:? "investigator_name"
        .!= maybe
          (error "missing investigator")
          toTitle
          (lookup (coerce investigator_code) allInvestigatorCards)
    meta <- o .:? "meta"
    taboo_id <- o .:? "taboo_id"
    url <- o .:? "url"
    pure $ ArkhamDBDecklist {..}
