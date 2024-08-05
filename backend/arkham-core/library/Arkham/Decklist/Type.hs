module Arkham.Decklist.Type where

import Arkham.Card.CardCode
import Arkham.Id
import Arkham.Prelude

data ArkhamDBDecklist = ArkhamDBDecklist
  { slots :: Map CardCode Int
  , sideSlots :: Map CardCode Int
  , investigator_code :: InvestigatorId
  , investigator_name :: Text
  , meta :: Maybe Text
  }
  deriving stock (Generic, Show, Eq, Data)
  deriving anyclass ToJSON

instance FromJSON ArkhamDBDecklist where
  parseJSON = withObject "ArkhamDBDecklist" $ \o -> do
    slots <- o .: "slots"
    sideSlots <- (o .: "sideSlots" <|> pure mempty)
    investigator_code <- o .: "investigator_code"
    investigator_name <- o .: "investigator_name"
    meta <- o .:? "meta"
    pure $ ArkhamDBDecklist {..}
