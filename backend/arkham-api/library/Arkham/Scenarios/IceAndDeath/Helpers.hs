module Arkham.Scenarios.IceAndDeath.Helpers where

import Arkham.Ability
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Helpers.Log (getCampaignLog)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.I18n
import Arkham.Id
import Arkham.Layout
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Source
import Data.Map.Strict qualified as Map

scenarioI18n :: Int -> (HasI18n => a) -> a
scenarioI18n n a = campaignI18n $ scope ("iceAndDeath.part" <> tshow n) a

iceAndDeathLayout :: [GridTemplateRow]
iceAndDeathLayout =
  [ "trefoil  .       .     .         .        .         plus"
  , "trefoil  .       .     moon      .        .         plus"
  , ".        droplet .     moon      .        equals    ."
  , ".        droplet .     .         .        equals    ."
  , ".        .       heart .         triangle .         ."
  , ".        .       heart .         triangle .         ."
  , ".        .       .     circle    .        .         ."
  , ".        star    .     circle    .        hourglass ."
  , ".        star    .     diamond   .        hourglass ."
  , ".        .       .     diamond   .        .         ."
  , ".        .       .     square    .        .         ."
  , ".        .       .     square    .        .         ."
  , ".        .       .     squiggle  .        .         ."
  , ".        .       .     squiggle  .        .         ."
  ]

placeSetAsideConnectedAbility :: (Sourceable a, HasCardCode a) => a -> Int -> Ability
placeSetAsideConnectedAbility a n = mkAbility a n $ forced $ Enters #after You $ ConnectedToSetAsideLocation

placeSetAsideConnectedLocations :: ReverseQueue m => LocationId -> m ()
placeSetAsideConnectedLocations lid = do
  symbol <- field LocationPrintedSymbol lid
  let isConnected = elem symbol . cdLocationConnections . toCardDef
  setAsideLocations <- filter isConnected <$> getSetAsideCardsMatching #location
  traverse_ placeLocation_ setAsideLocations

camps :: Map CardCode CampaignLogKey
camps =
  mapFromList
    [ (Locations.crashSite.cardCode, Camp_CrashSite)
    , (Locations.frozenShores.cardCode, Camp_FrozenShores)
    , (Locations.treacherousPath.cardCode, Camp_TreacherousPath)
    , (Locations.precariousIceSheet.cardCode, Camp_PrecariousIceSheet)
    , (Locations.broadSnowdrifts.cardCode, Camp_BroadSnowdrifts)
    , (Locations.icyWastes.cardCode, Camp_IcyWastes)
    , (Locations.rockyCrags.cardCode, Camp_RockyCrags)
    , (Locations.snowGraves.cardCode, Camp_SnowGraves)
    , (Locations.icebreakerLanding.cardCode, Camp_IcebreakerLanding)
    , (Locations.frigidCave.cardCode, Camp_FrigidCave)
    , (Locations.barrierCamp.cardCode, Camp_BarrierCamp)
    , (Locations.remnantsOfLakesCamp.cardCode, Camp_RemnantsOfLakesCamp)
    , (Locations.crystallineCavern.cardCode, Camp_CrystallineCavern)
    ]

getCamp :: HasGame m => m (Maybe CardDef)
getCamp = do
  rs <- campaignLogRecorded <$> getCampaignLog
  pure $ go rs (Map.toList camps) >>= lookupCardDef
 where
  go _ [] = Nothing
  go rs ((cc, k) : xs) = if k `member` rs then Just cc else go rs xs

getCurrentShelterValue :: HasGame m => m (Maybe Int)
getCurrentShelterValue = join . fmap getShelterValue <$> getCamp
