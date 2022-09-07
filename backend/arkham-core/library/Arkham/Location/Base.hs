module Arkham.Location.Base where

import Arkham.Prelude

import Arkham.Card
import Arkham.Cost
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Json
import Arkham.LocationSymbol
import Arkham.Matcher ( LocationMatcher (..) )

data LocationAttrs = LocationAttrs
  { locationId :: LocationId
  , locationCardCode :: CardCode
  , locationLabel :: Text
  , locationRevealClues :: GameValue Int
  , locationClues :: Int
  , locationDoom :: Int
  , locationHorror :: Int
  , locationResources :: Int
  , locationShroud :: Int
  , locationRevealed :: Bool
  , locationInvestigators :: HashSet InvestigatorId
  , locationEnemies :: HashSet EnemyId
  , locationSymbol :: LocationSymbol
  , locationRevealedSymbol :: LocationSymbol
  , locationConnectedMatchers :: [LocationMatcher]
  , locationRevealedConnectedMatchers :: [LocationMatcher]
  , locationTreacheries :: HashSet TreacheryId
  , locationEvents :: HashSet EventId
  , locationAssets :: HashSet AssetId
  , locationDirections :: HashMap Direction LocationId
  , locationConnectsTo :: HashSet Direction
  , locationCardsUnderneath :: [Card]
  , locationCostToEnterUnrevealed :: Cost
  , locationCanBeFlipped :: Bool
  -- We need to track if a location has no clues because timings will interact
  -- with the location being revealed and claim there are no clues before they
  -- are placed.
  , locationWithoutClues :: Bool
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LocationAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "location"

instance FromJSON LocationAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "location"
