module Arkham.Scenario.Scenarios.DisappearanceAtTheTwilightEstate (
  DisappearanceAtTheTwilightEstate (..),
  disappearanceAtTheTwilightEstate,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.ChaosBag
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (enemyAt)
import Arkham.Message (chooseDecks)
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Scenario.Helpers
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DisappearanceAtTheTwilightEstate.Story
import Arkham.Treachery.Cards qualified as Treacheries

newtype DisappearanceAtTheTwilightEstate = DisappearanceAtTheTwilightEstate ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disappearanceAtTheTwilightEstate
  :: Difficulty -> DisappearanceAtTheTwilightEstate
disappearanceAtTheTwilightEstate difficulty =
  scenario
    DisappearanceAtTheTwilightEstate
    "05043"
    "Disappearance at the Twilight Estate"
    difficulty
    [ ".             .          office         .             ."
    , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
    , ".             .          entryHall      .             ."
    ]

instance HasChaosTokenValue DisappearanceAtTheTwilightEstate where
  getChaosTokenValue iid chaosTokenFace (DisappearanceAtTheTwilightEstate attrs) =
    case chaosTokenFace of
      Skull -> pure $ toChaosTokenValue attrs Skull 3 5
      otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DisappearanceAtTheTwilightEstate where
  runMessage msg s@(DisappearanceAtTheTwilightEstate attrs) = runQueueT $ case msg of
    StandaloneSetup -> do
      setChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> runScenarioSetup DisappearanceAtTheTwilightEstate attrs do
      -- At Death's Doorstep is only locations so we will manually gather
      gather Set.DisappearanceAtTheTwilightEstate
      gather Set.InexorableFate
      gather Set.RealmOfDeath
      gather Set.SpectralPredators
      gather Set.TrappedSpirits
      gather Set.TheWatcher
      gather Set.ChillingCold

      setActDeck [Acts.theDisappearance]
      setAgendaDeck [Agendas.judgementXX]

      office <- place Locations.officeSpectral
      billiardsRoom <- place Locations.billiardsRoomSpectral
      victorianHalls <- place Locations.victorianHallsSpectral
      balcony <- place Locations.balconySpectral
      entryHall <- place Locations.entryHallSpectral

      placeAll [Locations.trophyRoomSpectral, Locations.masterBedroomSpectral]

      enemyAt_ Enemies.theSpectralWatcher entryHall

      selectForMaybeM (investigatorIs Investigators.gavriellaMizrah) \gavriella -> do
        moveTo_ attrs gavriella victorianHalls
        removeOneOf Treacheries.fateOfAllFools

      selectForMaybeM (investigatorIs Investigators.jeromeDavids) \jerome -> do
        moveTo_ attrs jerome office
        tid1 <- getRandom
        obscuringFog <- genCard Treacheries.obscuringFog
        push $ AttachStoryTreacheryTo tid1 obscuringFog (toTarget office)
        enemyAt_ Enemies.netherMist office

      selectForMaybeM (investigatorIs Investigators.valentinoRivas) \valentino -> do
        moveTo_ attrs valentino billiardsRoom
        placeEnemy Enemies.shadowHound (InThreatArea valentino)
        removeOneOf Treacheries.terrorInTheNight

      selectForMaybeM (investigatorIs Investigators.pennyWhite) \penny -> do
        moveTo_ attrs penny balcony
        placeEnemy Enemies.wraith (InThreatArea penny)
        removeOneOf Treacheries.whispersInTheDark

      push $ SetupStep (toTarget attrs) 2
    SetupStep (isTarget attrs -> True) 2 -> do
      agendaId <- selectJust AnyAgenda

      whenAny (investigatorIs Investigators.gavriellaMizrah) do
        theSpectralWatcher <- selectJust $ enemyIs Enemies.theSpectralWatcher
        nonAttackEnemyDamage attrs 1 theSpectralWatcher

      whenAny (investigatorIs Investigators.valentinoRivas) do
        terrorInTheNight <- genCard Treacheries.terrorInTheNight
        tid1 <- getRandom
        push $ AttachStoryTreacheryTo tid1 terrorInTheNight (toTarget agendaId)

      whenAny (investigatorIs Investigators.pennyWhite) do
        whispersInTheDark <- genCard Treacheries.whispersInTheDark
        tid2 <- getRandom
        push $ AttachStoryTreacheryTo tid2 whispersInTheDark (toTarget agendaId)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Skull ->
          getSkillTestAction >>= traverse_ \action ->
            when (action `elem` [#fight, #evade]) $ runHauntedAbilities iid
        _ -> pure ()
      pure s
    ScenarioResolution _ -> do
      story noResolution
      recordCount PiecesOfEvidenceWereLeftBehind =<< selectSum ActClues AnyAct
      players <- allPlayers
      push $ chooseDecks players
      endOfScenario
      pure s
    _ -> DisappearanceAtTheTwilightEstate <$> liftRunMessage msg attrs
