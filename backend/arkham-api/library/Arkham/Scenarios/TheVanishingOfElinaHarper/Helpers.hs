module Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers where

import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (Message (..))
import Arkham.Message.Lifted (ReverseQueue, recordSetInsert)
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Trait (Trait (Suspect))

notKidnapper :: EnemyMatcher
notKidnapper = not_ (EnemyWithModifier $ ScenarioModifier "kidnapper") <> withTrait Suspect

data Meta = Meta {kidnapper :: Card, hideout :: Card}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getKidnapper :: (HasCallStack, HasGame m) => m Card
getKidnapper = kidnapper <$> getScenarioMeta

getHideout :: (HasCallStack, HasGame m) => m Card
getHideout = hideout <$> getScenarioMeta

outForBlood :: ReverseQueue m => EnemyId -> m ()
outForBlood = recordSetInsert OutForBlood . only <=< field EnemyCardCode

suspects :: NonEmpty CardDef
suspects =
  Enemies.brianBurnhamWantsOut
    :| [ Enemies.otheraGilmanProprietessOfTheHotel
       , Enemies.joyceLittleBookshopOwner
       , Enemies.barnabasMarshTheChangeIsUponHim
       , Enemies.zadokAllenDrunkAndDisorderly
       , Enemies.robertFriendlyDisgruntledDockworker
       ]

hideouts :: NonEmpty CardDef
hideouts =
  Locations.innsmouthJail
    :| [ Locations.shorewardSlums
       , Locations.sawboneAlley
       , Locations.theHouseOnWaterStreet
       , Locations.esotericOrderOfDagon
       , Locations.newChurchGreen
       ]

crossOutLead :: ReverseQueue m => Card -> m ()
crossOutLead lead =
  when (cardMatch lead (toList suspects <> toList hideouts)) do
    push $ ForTarget (StoryTarget $ StoryId $ Stories.findingAgentHarper.cardCode) (RevealCard lead.id)

getPossibleSuspects :: (HasCallStack, HasGame m) => m [CardDef]
getPossibleSuspects =
  filterM
    ( \suspect ->
        andM [selectNone $ enemyIs suspect, selectNone $ VictoryDisplayCardMatch (basic $ cardIs suspect)]
    )
    (toList suspects)

getPossibleHideouts :: (HasCallStack, HasGame m) => m [CardDef]
getPossibleHideouts =
  filterM
    ( \hideout ->
        andM
          [selectNone $ locationIs hideout, selectNone $ VictoryDisplayCardMatch (basic $ cardIs hideout)]
    )
    (toList hideouts)

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = withI18n $ scope "theInnsmouthConspiracy" $ scope "theVanishingOfElinaHarper" a
