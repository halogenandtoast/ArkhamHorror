module Arkham.Campaigns.TheInnsmouthConspiracy.Helpers where

import Arkham.Ability
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
import Arkham.Message (Message (CreateEffect))
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
needsAir a n = restricted a n (youExist $ at_ FullyFloodedLocation) $ forced $ TurnBegins #when You

getFloodLevel :: HasGame m => LocationId -> m FloodLevel
getFloodLevel = fieldWithDefault Unflooded LocationFloodLevel

struggleForAir :: (Sourceable a, HasQueue Message m) => a -> InvestigatorId -> m ()
struggleForAir a iid = push $ CreateEffect $ makeEffectBuilder "noair" Nothing a iid

whenRecoveredMemory :: HasGame m => Memory -> m () -> m ()
whenRecoveredMemory memory action = do
  hasMemory <- inRecordSet memory MemoriesRecovered
  when hasMemory action

recoverMemory :: ReverseQueue m => Memory -> m ()
recoverMemory memory = recordSetInsert MemoriesRecovered [memory]

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "theInnsmouthConspiracy" a
