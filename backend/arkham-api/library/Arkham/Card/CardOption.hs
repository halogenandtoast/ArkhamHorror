module Arkham.Card.CardOption where

import Arkham.Prelude
import Control.Monad.Fail (fail)

{- | A configuration option a card offers its controller.

Options are player preferences about how the game should prompt them, not game
rules. A card declares its options on its 'CardDef' ('cdOptions'); the chosen
values live per-investigator in 'Arkham.Card.Settings.PerCardSettings'.

Nothing here carries display text — the frontend renders labels from i18n keyed
by @cardOption.\<cardCode\>.\<key\>@, so translations stay in the frontend.
-}
data CardOptionType
  = -- | default
    OptionToggle Bool
  | -- | value ids, default
    OptionChoice [Text] Text
  deriving stock (Show, Ord, Eq, Generic, Data)

instance ToJSON CardOptionType where
  toJSON = \case
    OptionToggle def -> object ["tag" .= String "toggle", "default" .= def]
    OptionChoice vals def -> object ["tag" .= String "choice", "values" .= vals, "default" .= def]

instance FromJSON CardOptionType where
  parseJSON = withObject "CardOptionType" \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "toggle" -> OptionToggle <$> o .: "default"
      "choice" -> OptionChoice <$> o .: "values" <*> o .: "default"
      other -> fail $ "unknown CardOptionType: " <> show other

data CardOption = CardOption
  { cardOptionKey :: Text
  , cardOptionType :: CardOptionType
  , cardOptionAbility :: Maybe Int
  {- ^ The ability this option scopes to, if any. The UI nests an ability's
  options under that ability's printed text; options with 'Nothing' apply to
  the card as a whole and are listed on their own.
  -}
  }
  deriving stock (Show, Ord, Eq, Generic, Data)

instance ToJSON CardOption where
  toJSON (CardOption k t a) =
    object $ ["key" .= k, "type" .= t] <> maybe [] (\n -> ["ability" .= n]) a

instance FromJSON CardOption where
  parseJSON = withObject "CardOption" \o ->
    CardOption <$> o .: "key" <*> o .: "type" <*> o .:? "ability"

-- | A value a player has chosen for a 'CardOption'.
data OptionValue
  = OptionBool Bool
  | OptionText Text
  deriving stock (Show, Ord, Eq, Generic, Data)

instance ToJSON OptionValue where
  toJSON = \case
    OptionBool b -> toJSON b
    OptionText t -> toJSON t

instance FromJSON OptionValue where
  parseJSON v = (OptionBool <$> parseJSON v) <|> (OptionText <$> parseJSON v)

cardToggle :: Text -> Bool -> CardOption
cardToggle k def = CardOption k (OptionToggle def) Nothing

cardChoice :: Text -> [Text] -> Text -> CardOption
cardChoice k vals def = CardOption k (OptionChoice vals def) Nothing

-- | Scope an option to one of the card's abilities, so the UI nests it there.
forAbility :: Int -> CardOption -> CardOption
forAbility n option = option {cardOptionAbility = Just n}

cardOptionDefault :: CardOption -> OptionValue
cardOptionDefault (CardOption _ t _) = case t of
  OptionToggle b -> OptionBool b
  OptionChoice _ d -> OptionText d

optionValueToBool :: OptionValue -> Bool
optionValueToBool = \case
  OptionBool b -> b
  OptionText t -> t `notElem` ["", "false", "no", "off"]
