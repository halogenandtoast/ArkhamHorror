{-# LANGUAGE TemplateHaskell #-}

module Arkham.Location.Base where

import Arkham.Prelude

import Arkham.Card
import Arkham.Cost
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.Key
import Arkham.Location.Brazier
import Arkham.Location.BreachStatus
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.LocationSymbol
import Arkham.Matcher (IsLocationMatcher (..), LocationMatcher (..))
import Arkham.SkillType
import Arkham.Token
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.TH
import Data.Map.Strict qualified as Map
import GHC.Records

data LocationAttrs = LocationAttrs
  { locationId :: LocationId
  , locationCardCode :: CardCode
  , locationCardId :: CardId
  , locationLabel :: Text
  , locationRevealClues :: GameValue
  , locationTokens :: Tokens
  , locationShroud :: Maybe Int
  , locationRevealed :: Bool
  , locationSymbol :: LocationSymbol
  , locationRevealedSymbol :: LocationSymbol
  , locationConnectedMatchers :: [LocationMatcher]
  , locationRevealedConnectedMatchers :: [LocationMatcher]
  , locationDirections :: Map Direction [LocationId]
  , locationConnectsTo :: Set Direction
  , locationCardsUnderneath :: [Card]
  , locationCostToEnterUnrevealed :: Cost
  , locationCanBeFlipped :: Bool
  , locationInvestigateSkill :: SkillType
  , locationInFrontOf :: Maybe InvestigatorId
  , locationKeys :: Set ArkhamKey
  , locationFloodLevel :: Maybe FloodLevel
  , locationBrazier :: Maybe Brazier
  , locationBreaches :: Maybe BreachStatus
  , -- We need to track if a location has no clues because timings will interact
    -- with the location being revealed and claim there are no clues before they
    -- are placed. TODO: this could be a hasBeenRevealed bool
    locationWithoutClues :: Bool
  , locationMeta :: Value
  , locationGlobalMeta :: Map Aeson.Key Value
  , locationPosition :: Maybe Pos
  }
  deriving stock (Show, Eq)

instance IsLocationMatcher LocationAttrs where
  toLocationMatcher = LocationWithId . locationId

instance AsId LocationAttrs where
  type IdOf LocationAttrs = LocationId
  asId = locationId

locationClues :: LocationAttrs -> Int
locationClues = countTokens Clue . locationTokens

locationDoom :: LocationAttrs -> Int
locationDoom = countTokens Doom . locationTokens

locationHorror :: LocationAttrs -> Int
locationHorror = countTokens Horror . locationTokens

locationDamage :: LocationAttrs -> Int
locationDamage = countTokens Damage . locationTokens

locationResources :: LocationAttrs -> Int
locationResources = countTokens Resource . locationTokens

instance HasField "tokens" LocationAttrs Tokens where
  getField = locationTokens

instance HasField "token" LocationAttrs (Token -> Int) where
  getField a tType = countTokens tType a.tokens

instance HasField "directions" LocationAttrs (Map Direction [LocationId]) where
  getField = locationDirections

instance HasField "leftOf" LocationAttrs (Maybe [LocationId]) where
  getField = lookup LeftOf . locationDirections

instance HasField "meta" LocationAttrs Value where
  getField = locationMeta

instance HasField "global" LocationAttrs (Aeson.Key -> Maybe Value) where
  getField l k = lookup k (locationGlobalMeta l)

instance HasField "position" LocationAttrs (Maybe Pos) where
  getField = locationPosition

instance HasField "row" LocationAttrs (Maybe Int) where
  getField l = positionRow <$> locationPosition l

instance HasField "revealed" LocationAttrs Bool where
  getField = locationRevealed

instance HasField "unrevealed" LocationAttrs Bool where
  getField = not . locationRevealed

instance HasField "floodLevel" LocationAttrs (Maybe FloodLevel) where
  getField = locationFloodLevel

instance HasField "label" LocationAttrs Text where
  getField = locationLabel

instance HasField "clues" LocationAttrs Int where
  getField = locationClues

instance HasField "keys" LocationAttrs (Set ArkhamKey) where
  getField = locationKeys

makeLensesWith suffixedFields ''LocationAttrs

setMeta :: ToJSON a => a -> LocationAttrs -> LocationAttrs
setMeta a = metaL .~ toJSON a

getLocationMeta :: FromJSON a => LocationAttrs -> Maybe a
getLocationMeta attrs = case fromJSON attrs.meta of
  Error _ -> Nothing
  Success v' -> Just v'

getLocationMetaDefault :: FromJSON a => a -> LocationAttrs -> a
getLocationMetaDefault def = fromMaybe def . getLocationMeta

$(deriveToJSON (aesonOptions $ Just "location") ''LocationAttrs)

instance FromJSON LocationAttrs where
  parseJSON = withObject "Location" \o -> do
    locationId <- o .: "id"
    locationCardCode <- o .: "cardCode"
    locationCardId <- o .: "cardId"
    locationLabel <- o .: "label"
    locationRevealClues <- o .: "revealClues"
    locationTokens <- o .: "tokens"
    locationShroud <- o .:? "shroud"
    locationRevealed <- o .: "revealed"
    locationSymbol <- o .: "symbol"
    locationRevealedSymbol <- o .: "revealedSymbol"
    locationConnectedMatchers <- o .: "connectedMatchers"
    locationRevealedConnectedMatchers <- o .: "revealedConnectedMatchers"
    locationDirections <- o .: "directions" <|> (Map.map (: []) <$> o .: "directions")
    locationConnectsTo <- o .: "connectsTo"
    locationCardsUnderneath <- o .: "cardsUnderneath"
    locationCostToEnterUnrevealed <- o .: "costToEnterUnrevealed"
    locationCanBeFlipped <- o .: "canBeFlipped"
    locationInvestigateSkill <- o .: "investigateSkill"
    locationInFrontOf <- o .:? "inFrontOf"
    locationKeys <- o .: "keys"
    locationFloodLevel <- o .:? "floodLevel"
    locationBrazier <- o .:? "brazier"
    locationBreaches <- o .:? "breaches"
    locationWithoutClues <- o .: "withoutClues"
    locationMeta <- o .: "meta"
    locationGlobalMeta <- o .:? "globalMeta" .!= mempty
    locationPosition <- o .:? "position"

    pure LocationAttrs {..}
