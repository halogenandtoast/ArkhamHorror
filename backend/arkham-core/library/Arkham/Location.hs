{-# LANGUAGE TemplateHaskell #-}
module Arkham.Location
  ( module Arkham.Location
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Card.Id
import Arkham.Classes
import Arkham.Direction
import Arkham.Id
import Arkham.Helpers.Modifiers
import Arkham.Label qualified as L
import Arkham.Location.Locations
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Name
import Arkham.Query
import Arkham.SkillTest
import Arkham.Trait (Trait)
import Data.HashSet qualified as HashSet
import Data.UUID (nil)
import Data.Aeson.TH

$(buildEntity "Location")

$(deriveJSON defaultOptions ''Location)

createLocation :: IsCard a => a -> Location
createLocation a = lookupLocation (toCardCode a) (LocationId $ toCardId a)

toLocationSymbol :: Location -> LocationSymbol
toLocationSymbol = locationSymbol . toAttrs

toLocationLabel :: Location -> L.Label
toLocationLabel = L.Label . locationLabel . toAttrs

instance HasCardCode Location where
  toCardCode = toCardCode . toAttrs

instance HasAbilities Location where
  getAbilities = $(entityF "Location" "getAbilities")

instance HasModifiersFor Location where
  getModifiersFor = $(entityF2 "Location" "getModifiersFor")

instance RunMessage Location where
  runMessage msg l = do
    modifiers' <- getModifiers (toSource l) (toTarget l)
    let msg' = if Blank `elem` modifiers' then Blanked msg else msg
    $(entityRunMessage "Location") msg' l

instance Entity Location where
  type EntityId Location = LocationId
  type EntityAttrs Location = LocationAttrs
  toId = toId . toAttrs
  toAttrs = $(entityF "Location" "toAttrs")

instance Named Location where
  toName = toName . toAttrs

instance Named (Unrevealed Location) where
  toName (Unrevealed l) = toName . Unrevealed $ toAttrs l

instance TargetEntity Location where
  toTarget = toTarget . toAttrs
  isTarget = isTarget . toAttrs

instance SourceEntity Location where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

lookupLocationStub :: CardCode -> Location
lookupLocationStub = ($ LocationId (CardId nil)) . lookupLocation

lookupLocation :: CardCode -> (LocationId -> Location)
lookupLocation lid =
  fromJustNote ("Unknown location: " <> show lid) $ lookup lid allLocations

allLocations :: HashMap CardCode (LocationId -> Location)
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
