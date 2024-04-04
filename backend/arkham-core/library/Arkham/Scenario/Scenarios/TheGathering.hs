module Arkham.Scenario.Scenarios.TheGathering where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.NightOfTheZealot.ChaosBag
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Runner hiding (
  assignDamageAndHorror,
  assignHorror,
  findAndDrawEncounterCard,
  placeLocationCard,
  story,
 )
import Arkham.Scenario.Setup
import Arkham.Trait (Trait (Ghoul))

newtype TheGathering = TheGathering ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theGathering :: Difficulty -> TheGathering
theGathering difficulty =
  scenario TheGathering "01104" "The Gathering" difficulty
    $ [ "   .   attic   .     "
      , " study hallway parlor"
      , "   .   cellar  .     "
      ]

instance HasChaosTokenValue TheGathering where
  getChaosTokenValue iid chaosTokenFace (TheGathering attrs) = case chaosTokenFace of
    Skull -> do
      ghoulCount <- selectCount $ enemyAtLocationWith iid <> withTrait Ghoul
      pure $ toChaosTokenValue attrs Skull ghoulCount 2
    Cultist -> pure $ ChaosTokenValue Cultist (mwhen (isEasyStandard attrs) (NegativeModifier 1))
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

theGatheringAgendaDeck :: [CardDef]
theGatheringAgendaDeck = [Agendas.whatsGoingOn, Agendas.riseOfTheGhouls, Agendas.theyreGettingOut]

instance RunMessage TheGathering where
  runMessage msg s@(TheGathering attrs) = runQueueT $ case msg of
    SetChaosTokensForScenario -> do
      pushWhenM getIsStandalone $ SetChaosTokens (chaosBagContents $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      story $ i18nWithTitle "nightOfTheZealot.theGathering.intro"
      pure s
    Setup -> runScenarioSetup TheGathering attrs do
      gather Set.TheGathering
      gather Set.Rats
      gather Set.Ghouls
      gather Set.StrikingFear
      gather Set.AncientEvils
      gather Set.ChillingCold

      setAside
        [ Enemies.ghoulPriest
        , Assets.litaChantler
        , Locations.hallway
        , Locations.attic
        , Locations.cellar
        , Locations.parlor
        ]

      setAgendaDeck theGatheringAgendaDeck
      setActDeck [Acts.trapped, Acts.theBarrier, Acts.whatHaveYouDone]

      study <- place Locations.study
      startAt study
    ResolveChaosToken _ Cultist iid -> do
      pushWhen (isHardExpert attrs) $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      whenM (selectAny $ enemyAtLocationWith iid <> withTrait Ghoul)
        $ assignDamageAndHorror iid Tablet 1 (byDifficulty attrs 0 1)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull | isHardExpert attrs -> findAndDrawEncounterCard iid (isEnemyCard $ withTrait Ghoul)
        Cultist -> assignHorror iid (ChaosTokenSource token) (byDifficulty attrs 1 2)
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      resigned <- select ResignedInvestigator
      leadId <- getLead
      let
        chooseToAddLita killed =
          addCampaignCardToDeckChoice
            (if leadId `elem` killed then resigned else [leadId])
            Assets.litaChantler
      case resolution of
        NoResolution -> do
          story $ i18n "nightOfTheZealot.theGathering.resolutions.noResolution"
          record YourHouseIsStillStanding
          record GhoulPriestIsStillAlive
          chooseToAddLita []
          allGainXpWithBonus attrs 2
        Resolution 1 -> do
          story $ i18nWithTitle "nightOfTheZealot.theGathering.resolutions.resolution1"
          record YourHouseHasBurnedToTheGround
          chooseToAddLita []
          sufferMentalTrauma leadId 1
          allGainXpWithBonus attrs 2
        Resolution 2 -> do
          story $ i18nWithTitle "nightOfTheZealot.theGathering.resolutions.resolution2"
          record YourHouseIsStillStanding
          gainXp leadId attrs 1
          allGainXpWithBonus attrs 2
        Resolution 3 -> do
          -- TODO: missing rules
          -- \* handle new investigators
          story $ i18nWithTitle "nightOfTheZealot.theGathering.resolutions.resolution3"
          record LitaWasForcedToFindOthersToHelpHerCause
          record YourHouseIsStillStanding
          record GhoulPriestIsStillAlive

          killed <- killRemaining attrs
          when (notNull resigned) $ chooseToAddLita killed
        other -> throwIO $ UnknownResolution other

      endOfScenario
      pure s
    _ -> TheGathering <$> lift (runMessage msg attrs)
