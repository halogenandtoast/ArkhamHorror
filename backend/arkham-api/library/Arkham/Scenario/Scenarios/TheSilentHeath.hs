module Arkham.Scenario.Scenarios.TheSilentHeath (theSilentHeath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheSilentHeath.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Cave, Elite, Insect, Lair))

newtype TheSilentHeath = TheSilentHeath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSilentHeath :: Difficulty -> TheSilentHeath
theSilentHeath difficulty = scenario TheSilentHeath "10549" "The Silent Heath" difficulty []

instance HasChaosTokenValue TheSilentHeath where
  getChaosTokenValue iid tokenFace (TheSilentHeath attrs) = case tokenFace of
    Skull -> do
      inPlay <- selectCount $ EnemyWithTrait Insect
      inVictory <- selectCount $ VictoryDisplayCardMatch $ basic $ #enemy <> CardWithTrait Insect
      let n = inPlay + inVictory
      pure $ toChaosTokenValue attrs Skull ((n + 1) `div` 2) n
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheSilentHeath where
  runMessage msg s@(TheSilentHeath attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      pure s
    Setup -> runScenarioSetup TheSilentHeath attrs do
      setUsesGrid

      gather Set.TheSilentHeath
      gather Set.AgentsOfTheColour
      gather Set.Blight
      gather Set.HorrorsInTheRock
      gather Set.Refractions
      gather Set.Transfiguration
      gather Set.StrikingFear

      setScenarioDayAndTime
      day <- getCampaignDay
      time <- getCampaignTime

      case day of
        Day1 -> do
          gather Set.TheFirstDay
          placeStory $ case time of
            Day -> Stories.dayOne
            Night -> Stories.nightOne
        Day2 -> do
          gather Set.TheSecondDay
          placeStory $ case time of
            Day -> Stories.dayTwo
            Night -> Stories.nightTwo
        Day3 -> do
          gather Set.TheFinalDay
          placeStory $ case time of
            Day -> Stories.dayThree
            Night -> Stories.nightThree

      let
        agenda2 =
          case day of
            Day1 -> Agendas.desolationV1
            _ -> Agendas.desolationV2

      setAgendaDeck [Agendas.unsettlingSilence, agenda2]
      setActDeck [Acts.aLostLegacy, Acts.searchingTheHeath]

      ruins <- placeInGrid (Pos 0 0) Locations.pearlEstateRuins
      slope <- placeInGrid (Pos 2 0) Locations.ashenSlope
      grove <- placeInGrid (Pos 1 (-1)) Locations.crystalGrove

      startAt ruins

      setAside
        [ Enemies.broodQueenDyingMother
        , Enemies.crystalParasite
        , Enemies.crystalParasite
        , Locations.saltChamber
        , Locations.larvalTunnel
        , Locations.crystalNursery
        ]

      setAsideFacedown
        =<< shuffle
          [Assets.crystalRemainsTheChild, Assets.crystalRemainsTheFather, Assets.crystalRemainsTheMother]

      horrorsInTheRockLocations <- fromGathered (CardFromEncounterSet Set.HorrorsInTheRock <> #location)
      setAsideFacedown =<< sampleListN 3 horrorsInTheRockLocations
      void $ fromGathered (CardFromEncounterSet Set.HorrorsInTheRock)

      when (time == Day) $ case day of
        Day1 -> assetAt_ Assets.leahAtwoodTheValeCook ruins
        Day2 -> assetAt_ Assets.drRosaMarquezBestInHerField grove
        Day3 -> assetAt_ Assets.motherRachelKindlyMatron slope
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      cards <- select $ VictoryDisplayCardMatch $ basic $ #enemy <> not_ (CardWithTrait Elite)
      unless (null cards) $ chooseOneM iid do
        targets cards \card -> shuffleCardsIntoDeck Deck.EncounterDeck [card]
        labeledI "doNothing" nothing
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Cultist
      , isHardExpert attrs -> do
          cards <- select $ VictoryDisplayCardMatch $ basic $ #enemy <> not_ (CardWithTrait Elite)
          unless (null cards) $ chooseOneM iid do
            targets cards \card -> shuffleCardsIntoDeck Deck.EncounterDeck [card]
            labeledI "doNothing" nothing
          pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == Tablet
      , isEasyStandard attrs -> do
          enemies <- select $ EnemyWithTrait Insect <> enemyAtLocationWith iid
          chooseTargetM iid enemies \enemy -> do
            push $ EnemyEngageInvestigator enemy iid
            initiateEnemyAttack enemy Tablet iid
          pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      enemies <- select $ NearestEnemyTo iid (EnemyWithTrait Insect)
      chooseTargetM iid enemies \enemy -> do
        readyThis enemy
        sendMessage enemy HuntersMove
        sendMessage enemy (Do EnemiesAttack)
      pure s
    ResolveChaosToken _ ElderThing iid | isEasyStandard attrs -> do
      atLair <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Lair
      when atLair $ push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      atLair <- selectAny $ locationWithInvestigator iid <> LocationWithTrait Lair
      when atLair failSkillTest
      pure s
    ScenarioSpecific "codex" v -> scope "codex" do
      let (_iid :: InvestigatorId, _source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        Sigma -> do
          entry "broodQueen"
          createSetAsideEnemy_ Enemies.broodQueenDyingMother
            $ NearestLocationToMost Anywhere <> FirstLocation [LocationWithTrait Cave, Anywhere]
          doStep 1 msg
        _ -> pure ()
      pure s
    DoStep 1 (ScenarioSpecific "codex" v) -> scope "codex" do
      let (_iid :: InvestigatorId, _source :: Source, n :: Int) = toResult v
      case n of
        Sigma -> do
          loc <- selectJust $ LocationWithEnemy $ enemyIs Enemies.broodQueenDyingMother
          insects <- select $ VictoryDisplayCardMatch $ basic $ CardWithTrait Insect
          for_ insects \card -> do
            obtainCard card
            createEnemyAt_ card loc
        _ -> pure ()
      pure s
    _ -> TheSilentHeath <$> liftRunMessage msg attrs
