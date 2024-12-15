module Arkham.Campaigns.TheInnsmouthConspiracy.Helpers where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card.CardCode (HasCardCode)
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (HasQueue, push)
import Arkham.Effect.Types (makeEffectBuilder)
import Arkham.Helpers.Log hiding (recordSetInsert)
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Key
import Arkham.Location.FloodLevel
import Arkham.Location.Types (Field (LocationFloodLevel))
import Arkham.Matcher
import Arkham.Message (
  Message (CreateEffect, DecreaseFloodLevel, IncreaseFloodLevel, SetFloodLevel),
 )
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Types
import Arkham.Source
import Arkham.Target

placeUnrevealedKeyOn :: (ReverseQueue m, Targetable target) => target -> m ()
placeUnrevealedKeyOn target = do
  let
    unrevealed = \case
      UnrevealedKey _ -> True
      _ -> False
  unrevealedKeys <- filter unrevealed . setToList <$> scenarioField ScenarioSetAsideKeys
  for_ (nonEmpty unrevealedKeys) $ sample >=> placeKey (toTarget target)

data Suspect = BrianBurnham | BarnabasMarsh | OtheraGilman | ZadokAllen | JoyceLittle | RobertFriendly
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Hideout
  = InnsmouthJail
  | ShorewardSlums
  | SawboneAlley
  | TheHouseOnWaterStreet
  | EsotericOrderOfDagon
  | NewChurchGreen
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

needsAir :: (HasCardCode a, Sourceable a) => a -> Int -> Ability
needsAir a n =
  restricted
    a
    n
    ( youExist
        $ at_ FullyFloodedLocation
        <> not_ (InVehicleMatching $ assetIs Assets.fishingVessel)
        <> not_ (HasMatchingAsset $ assetIs Assets.divingSuit)
    )
    $ forced
    $ TurnBegins #when You

getFloodLevel :: HasGame m => LocationId -> m FloodLevel
getFloodLevel = fieldWithDefault Unflooded LocationFloodLevel

increaseThisFloodLevel
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> m ()
increaseThisFloodLevel location = push $ IncreaseFloodLevel (asId location)

decreaseThisFloodLevel
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> m ()
decreaseThisFloodLevel location = push $ DecreaseFloodLevel (asId location)

increaseThisFloodLevelTo
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> FloodLevel -> m ()
increaseThisFloodLevelTo location newFloodLevel = do
  floodLevel <- fieldWithDefault Unflooded LocationFloodLevel location
  when (floodLevel < newFloodLevel) do
    push $ SetFloodLevel (asId location) newFloodLevel

setThisFloodLevel
  :: (ReverseQueue m, AsId location, IdOf location ~ LocationId) => location -> FloodLevel -> m ()
setThisFloodLevel location level = push $ SetFloodLevel (asId location) level

struggleForAir :: (Sourceable a, HasGame m, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = do
  builder <- makeEffectBuilder "noair" Nothing a iid
  push $ CreateEffect builder

whenRecoveredMemory :: HasGame m => Memory -> m () -> m ()
whenRecoveredMemory memory action = whenM (hasMemory memory) action

hasMemory :: HasGame m => Memory -> m Bool
hasMemory memory = inRecordSet memory MemoriesRecovered

recoverMemory :: ReverseQueue m => Memory -> m ()
recoverMemory memory = recordSetInsert MemoriesRecovered [memory]

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theInnsmouthConspiracy" a
