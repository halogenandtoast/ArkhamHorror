module Arkham.Scenario.Scenarios.TheGathering where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner hiding (
  assignHorror,
  findAndDrawEncounterCard,
  placeLocationCard,
  story,
 )
import Arkham.Scenarios.TheGathering.Story
import Arkham.Trait qualified as Trait

newtype TheGathering = TheGathering ScenarioAttrs
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, NoThunks, Eq)

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
      ghoulCount <- selectCount $ enemyAtLocationWith iid <> withTrait Trait.Ghoul
      pure $ toChaosTokenValue attrs Skull ghoulCount 2
    Cultist -> pure $ ChaosTokenValue Cultist (mwhen (isEasyStandard attrs) (NegativeModifier 1))
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

theGatheringAgendaDeck :: [CardDef]
theGatheringAgendaDeck = [Agendas.whatsGoingOn, Agendas.riseOfTheGhouls, Agendas.theyreGettingOut]

instance RunMessage TheGathering where
  runMessage msg s@(TheGathering attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story theGatheringIntro
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.ghoulPriest]
          [ EncounterSet.TheGathering
          , EncounterSet.Rats
          , EncounterSet.Ghouls
          , EncounterSet.StrikingFear
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]

      setAsideCards <-
        genCards
          [ Enemies.ghoulPriest
          , Assets.litaChantler
          , Locations.hallway
          , Locations.attic
          , Locations.cellar
          , Locations.parlor
          ]

      agendas <- genCards theGatheringAgendaDeck
      acts <- genCards [Acts.trapped, Acts.theBarrier, Acts.whatHaveYouDone]

      setEncounterDeck encounterDeck
      setAgendaDeck
      setActDeck

      study <- placeLocationCard Locations.study
      reveal study
      moveAllTo attrs study

      TheGathering
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid -> do
      pushWhen (isHardExpert attrs) $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      pushWhenM (selectAny $ enemyAtLocationWith iid <> withTrait Trait.Ghoul)
        $ assignDamageAndHorror iid Tablet 1 (if isEasyStandard attrs then 0 else 1)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Skull | isHardExpert attrs -> findAndDrawEncounterCard iid (#enemy <> withTrait Trait.Ghoul)
        Cultist -> assignHorror iid (ChaosTokenSource token) (if isEasyStandard attrs then 1 else 2)
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      lead <- getLeadPlayer
      leadId <- getLeadInvestigatorId
      let allGainXp = pushAll =<< toGainXp attrs (getXpWithBonus 2)
      let chooseToAddLita = push $ addCampaignCardToDeckChoice lead [leadId] Assets.litaChantler
      case resolution of
        NoResolution -> do
          story noResolution
          record YourHouseIsStillStanding
          record GhoulPriestIsStillAlive
          chooseToAddLita
          allGainXp
        Resolution 1 -> do
          story resolution1
          record YourHouseHasBurnedToTheGround
          chooseToAddLita
          sufferMentalTrauma leadId 1
          allGainXp
        Resolution 2 -> do
          -- TODO: Combine gainXP and bonus so modifiers work
          story resolution2
          record YourHouseIsStillStanding
          gainXp leadId attrs 1
          allGainXp
        Resolution 3 -> do
          -- TODO: missing rules
          -- \* kill non-resigned investigators
          -- \* end campaign if none left
          -- \* handle new investigators
          -- \* handle lead being killed
          story resolution3
          record LitaWasForcedToFindOthersToHelpHerCause
          record YourHouseIsStillStanding
          record GhoulPriestIsStillAlive
          chooseToAddLita
        other -> throwIO $ UnknownResolution other

      endOfScenario
      pure s
    _ -> TheGathering <$> lift (runMessage msg attrs)
