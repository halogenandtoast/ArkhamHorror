{-# LANGUAGE TemplateHaskell #-}
module Arkham.Types.Location
  ( module Arkham.Types.Location
  ) where

import Arkham.Prelude

import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.Classes
import Arkham.Types.Direction
import Arkham.Types.Id
import qualified Arkham.Types.Label as L
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Cards
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Name
import Arkham.Types.Query
import Data.UUID (nil)

$(buildEntity "Location")

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (toCardCode a) (LocationId $ toCardId a)

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol = locationSymbol . toAttrs

toLocationLabel :: Location -> L.Label
toLocationLabel = L.Label . locationLabel . toAttrs

instance HasCardCode Location where
  toCardCode = toCardCode . toAttrs

instance IsCard Location where
  toCardId = toCardId . toAttrs

deriving anyclass instance (HasList Card env ExtendedCardMatcher, ActionRunner env) => HasActions env Location

instance
  ( HasPhase env
  , HasCount CardCount env InvestigatorId
  , HasCount ClueCount env LocationId
  , HasCount ResourceCount env InvestigatorId
  , HasId (Maybe StoryEnemyId) env CardCode
  , Query AssetMatcher env
  )
  => HasModifiersFor env Location where
  getModifiersFor = genericGetModifiersFor

instance
  ( HasSet UnengagedEnemyId env LocationId
  , HasSet EnemyId env EnemyMatcher
  , LocationRunner env
  )
  => RunMessage env Location where
  runMessage msg l = do
    modifiers' <- getModifiers (toSource l) (toTarget l)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    genericRunMessage msg' l

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs

instance Named Location where
  toName = toName . toAttrs

instance TargetEntity Location where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Location where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasCardDef Location where
  toCardDef = toCardDef . toAttrs

instance HasName env Location where
  getName = getName . toAttrs

instance HasVictoryPoints Location where
  getVictoryPoints l =
    let LocationAttrs { locationClues, locationRevealed } = toAttrs l
    in
      if locationClues == 0 && locationRevealed
        then cdVictoryPoints (toCardDef l)
        else Nothing

instance HasCount ResourceCount env Location where
  getCount = pure . ResourceCount . locationResources . toAttrs

instance HasCount ClueCount env Location where
  getCount = pure . ClueCount . locationClues . toAttrs

instance HasCount Shroud env Location where
  getCount = pure . Shroud . locationShroud . toAttrs

instance HasCount DoomCount env Location where
  getCount = pure . DoomCount . locationDoom . toAttrs

instance HasList UnderneathCard env Location where
  getList = getList . toAttrs

instance HasSet EnemyId env Location where
  getSet = pure . locationEnemies . toAttrs

instance HasSet TreacheryId env Location where
  getSet = pure . locationTreacheries . toAttrs

instance HasSet EventId env Location where
  getSet = pure . locationEvents . toAttrs

instance HasSet AssetId env Location where
  getSet = pure . locationAssets . toAttrs

instance HasSet InvestigatorId env Location where
  getSet = pure . locationInvestigators . toAttrs

instance HasSet ConnectedLocationId env Location where
  getSet =
    pure . mapSet ConnectedLocationId . locationConnectedLocations . toAttrs

instance HasId LocationId env Location where
  getId = pure . toId

instance HasId LocationSymbol env Location where
  getId = getId . toAttrs

instance HasId (Maybe LocationId) env (Direction, Location) where
  getId (dir, location') = getId (dir, toAttrs location')


lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

allLocations :: Map CardCode (LocationId -> Location)
allLocations =
  mapFromList
    $ map
        (cbCardCode &&& cbCardBuilder)
        $(buildEntityLookupList "Location")

isEmptyLocation :: Location -> Bool
isEmptyLocation =
  and . sequence [noInvestigatorsAtLocation, noEnemiesAtLocation]

noInvestigatorsAtLocation :: Location -> Bool
noInvestigatorsAtLocation l = null investigators'
  where investigators' = locationInvestigators $ toAttrs l

noEnemiesAtLocation :: Location -> Bool
noEnemiesAtLocation l = null enemies'
  where enemies' = locationEnemies $ toAttrs l

isRevealed :: Location -> Bool
isRevealed = locationRevealed . toAttrs
