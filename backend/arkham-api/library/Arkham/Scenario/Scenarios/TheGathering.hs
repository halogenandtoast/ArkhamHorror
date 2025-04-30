module Arkham.Scenario.Scenarios.TheGathering where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.NightOfTheZealot.Import
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.FlavorText (ikey)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Message.Lifted.Log
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheGathering.Helpers
import Arkham.Trait (Trait (Ghoul))

newtype TheGathering = TheGathering ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theGathering :: Difficulty -> TheGathering
theGathering difficulty =
  scenario
    TheGathering
    "01104"
    "The Gathering"
    difficulty
    [ "   .   attic   .     "
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
  runMessage msg s@(TheGathering attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      pure s
    Setup -> runScenarioSetup TheGathering attrs do
      setup $ ul do
        li "gatherSets"
        li "placeLocations"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"

      gather Set.TheGathering
      gather Set.Rats
      gather Set.Ghouls
      gather Set.StrikingFear
      gather Set.AncientEvils
      gather Set.ChillingCold

      setAgendaDeck theGatheringAgendaDeck
      setActDeck [Acts.trapped, Acts.theBarrier, Acts.whatHaveYouDone]

      setAside
        [ Enemies.ghoulPriest
        , Assets.litaChantler
        , Locations.hallway
        , Locations.attic
        , Locations.cellar
        , Locations.parlor
        ]

      startAt =<< place Locations.study
    ResolveChaosToken _ Cultist iid -> do
      when (isHardExpert attrs) $ drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      whenAny (enemyAtLocationWith iid <> withTrait Ghoul) do
        assignDamageAndHorror iid Tablet 1 (byDifficulty attrs 0 1)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull | isHardExpert attrs -> findAndDrawEncounterCard iid (isEnemyCard $ withTrait Ghoul)
        Cultist -> assignHorror iid (ChaosTokenSource token) (byDifficulty attrs 1 2)
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      resigned <- select ResignedInvestigator
      leadId <- getLead
      let
        chooseToAddLita killed = do
          let valids = if leadId `elem` killed then resigned else [leadId]
          valids' <- select $ InvestigatorCanAddCardsToDeck <> mapOneOf InvestigatorWithId valids
          unless (null valids') do
            addCampaignCardToDeckChoice valids' DoNotShuffleIn Assets.litaChantler
      case resolution of
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs $ toBonus "bonus" 2
          record YourHouseIsStillStanding
          record GhoulPriestIsStillAlive
          chooseToAddLita []
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXpWithBonus' attrs $ toBonus "bonus" 2
          record YourHouseHasBurnedToTheGround
          chooseToAddLita []
          sufferMentalTrauma leadId 1
        Resolution 2 -> do
          resolutionWithXp "resolution2" $ allGainXpWithBonus' attrs $ toBonus "bonus" 2
          record YourHouseIsStillStanding
          gainXp leadId attrs (ikey "xp.resolution2") 1
        Resolution 3 -> do
          story $ i18nWithTitle "resolution3"
          record LitaWasForcedToFindOthersToHelpHerCause
          record YourHouseIsStillStanding
          record GhoulPriestIsStillAlive

          remaining <- killRemaining attrs
          when (notNull resigned) $ chooseToAddLita remaining
        other -> throwIO $ UnknownResolution other

      endOfScenario
      pure s
    _ -> TheGathering <$> liftRunMessage msg attrs
