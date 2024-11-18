module Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers where

import Arkham.Ability
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
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
import Arkham.Message.Lifted (ReverseQueue, recordSetInsert, shuffleCardsIntoDeck, shuffleDeck)
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Story.Cards qualified as Stories
import Arkham.Target
import Arkham.Trait (Trait (Suspect))

getLeadsDeck :: HasGame m => m [Card]
getLeadsDeck = getScenarioDeck LeadsDeck

shuffleLeadsDeck :: ReverseQueue m => m ()
shuffleLeadsDeck = shuffleDeck LeadsDeck

shuffleIntoLeadsDeck
  :: (IsCard (Element cards), ReverseQueue m, MonoFoldable cards) => cards -> m ()
shuffleIntoLeadsDeck cs = shuffleCardsIntoDeck LeadsDeck $ map toCard $ toList cs

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
outForBlood = recordSetInsert OutForBlood . only . toJSON . asSuspect <=< field EnemyCard

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
scenarioI18n a = campaignI18n $ scope "theVanishingOfElinaHarper" a

scenarioTooltip :: Text -> Ability -> Ability
scenarioTooltip t ab = scenarioI18n $ withI18nTooltip t ab

asSuspect :: HasCardDef a => a -> Suspect
asSuspect (toCardDef -> def) =
  if
    | def.cardCode == Enemies.brianBurnhamWantsOut.cardCode -> BrianBurnham
    | def.cardCode == Enemies.barnabasMarshTheChangeIsUponHim.cardCode -> BarnabasMarsh
    | def.cardCode == Enemies.otheraGilmanProprietessOfTheHotel.cardCode -> OtheraGilman
    | def.cardCode == Enemies.zadokAllenDrunkAndDisorderly.cardCode -> ZadokAllen
    | def.cardCode == Enemies.joyceLittleBookshopOwner.cardCode -> JoyceLittle
    | def.cardCode == Enemies.robertFriendlyDisgruntledDockworker.cardCode -> RobertFriendly
    | otherwise -> error "wrong card"

asHideout :: HasCardDef a => a -> Hideout
asHideout (toCardDef -> def) =
  if
    | def.cardCode == Locations.innsmouthJail.cardCode -> InnsmouthJail
    | def.cardCode == Locations.shorewardSlums.cardCode -> ShorewardSlums
    | def.cardCode == Locations.sawboneAlley.cardCode -> SawboneAlley
    | def.cardCode == Locations.theHouseOnWaterStreet.cardCode -> TheHouseOnWaterStreet
    | def.cardCode == Locations.esotericOrderOfDagon.cardCode -> EsotericOrderOfDagon
    | def.cardCode == Locations.newChurchGreen.cardCode -> NewChurchGreen
    | otherwise -> error "wrong card"
