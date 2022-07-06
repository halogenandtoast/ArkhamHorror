module Arkham.Location.Base where

import Arkham.Prelude

import Arkham.Json
import Arkham.Direction
import Arkham.Id
import Arkham.Cost
import Arkham.Card
import Arkham.GameValue
import Arkham.LocationSymbol
import Arkham.Matcher (LocationMatcher(..))

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
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LocationAttrs where
  toJSON = genericToJSON $ aesonOptions $ Just "location"

instance FromJSON LocationAttrs where
  parseJSON = genericParseJSON $ aesonOptions $ Just "location"
