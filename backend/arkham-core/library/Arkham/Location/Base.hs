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
import Arkham.LocationSymbol
import Arkham.Matcher (IsLocationMatcher (..), LocationMatcher (..))
import Arkham.SkillType
import Arkham.Token
import Data.Aeson.TH
import GHC.Records

data LocationAttrs = LocationAttrs
  { locationId :: LocationId
  , locationCardCode :: CardCode
  , locationCardId :: CardId
  , locationLabel :: Text
  , locationRevealClues :: GameValue
  , locationTokens :: Tokens
  , locationShroud :: Int
  , locationRevealed :: Bool
  , locationSymbol :: LocationSymbol
  , locationRevealedSymbol :: LocationSymbol
  , locationConnectedMatchers :: [LocationMatcher]
  , locationRevealedConnectedMatchers :: [LocationMatcher]
  , locationDirections :: Map Direction LocationId
  , locationConnectsTo :: Set Direction
  , locationCardsUnderneath :: [Card]
  , locationCostToEnterUnrevealed :: Cost
  , locationCanBeFlipped :: Bool
  , locationInvestigateSkill :: SkillType
  , locationInFrontOf :: Maybe InvestigatorId
  , locationKeys :: Set ArkhamKey
  , locationBrazier :: Maybe Brazier
  , locationBreaches :: Maybe BreachStatus
  , -- We need to track if a location has no clues because timings will interact
    -- with the location being revealed and claim there are no clues before they
    -- are placed. TODO: this could be a hasBeenRevealed bool
    locationWithoutClues :: Bool
  , locationMeta :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NoThunks, NFData)

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

instance HasField "meta" LocationAttrs Value where
  getField = locationMeta

instance HasField "revealed" LocationAttrs Bool where
  getField = locationRevealed

instance HasField "clues" LocationAttrs Int where
  getField = locationClues

instance HasField "keys" LocationAttrs (Set ArkhamKey) where
  getField = locationKeys

makeLensesWith suffixedFields ''LocationAttrs

$(deriveJSON (aesonOptions $ Just "location") ''LocationAttrs)
