module Arkham.Scenario.Scenarios.TheSilentHeath (theSilentHeath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Campaigns.TheFeastOfHemlockVale.Key
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers (unDeck)
import Arkham.Helpers.Act
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheSilentHeath.Helpers
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
    PreScenarioSetup -> scope "intro" do
      day <- getCampaignDay
      time <- getCampaignTime
      let isNight = time == Night
      flavor do
        setTitle "title"
        p.basic "body"
        ul $ li.nested.validate isNight "nightSkip" do
          li.validate (not isNight && day == Day1) "day1"
          li.validate (not isNight && day == Day2) "day2"
          li.validate (not isNight && day == Day3) "day3"
      case (day, time) of
        (Day1, Day) -> do
          story $ i18nWithTitle "intro1"
          story $ i18nWithTitle "intro5"
        (Day2, Day) -> do
          story $ i18nWithTitle "intro2"
          story $ i18nWithTitle "intro5"
        (Day3, Day) -> do
          story $ i18nWithTitle "intro3"
          story $ i18nWithTitle "intro5"
        _ -> story $ i18nWithTitle "intro4"
      pure s
    Setup -> runScenarioSetup TheSilentHeath attrs do
      setScenarioDayAndTime
      day <- getCampaignDay
      time <- getCampaignTime

      setup $ ul do
        li "gatherSets"
        li "currentDaySet"
        li.nested "currentDayMarker" do
          li.validate (day == Day1) "desolationV1"
          li.validate (day /= Day1) "desolationV2"
        li.nested "locations" do
          li "startAt"
        li "setAside"
        li "crystalRemains"
        li "horrorsInTheRock"
        li.nested.validate (time == Day) "residents" do
          if time == Day
            then do
              li.validate (day == Day1) "leahAtwood"
              li.validate (day == Day2) "drRosaMarquez"
              li.validate (day == Day3) "motherRachel"
              li "removeResidents"
            else do
              li "leahAtwood"
              li "drRosaMarquez"
              li "motherRachel"
              li "removeResidents"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      setUsesGrid

      gather Set.TheSilentHeath
      gather Set.AgentsOfTheColour
      gather Set.Blight
      gather Set.HorrorsInTheRock
      gather Set.Refractions
      gather Set.Transfiguration
      gather Set.StrikingFear

      setupHemlockDay day time

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
      let (iid :: InvestigatorId, source :: Source, n :: Int) = toResult v
      let entry x = scope x $ flavor $ setTitle "title" >> p.green "body"
      case n of
        1 -> do
          codexFinished 1
          entry "motherRachel"
          increaseRelationshipLevel MotherRachel 1
          interludeXpAll (toBonus "bonus" 1)
        2 -> do
          codexFinished 2
          entry "leahAtwood"
          record LeahSearchedThePearlRuins
          eachInvestigator \iid' -> gainClues iid' source 1
        Theta -> do
          step <- getCurrentActStep
          if step == 1
            then do
              codexFinishedUntilNewAct Theta
              scope "drRosaMarquez" $ flavor $ setTitle "title" >> p.green "act1"
              drawCards iid source 3
            else do
              codexFinished Theta
              scope "drRosaMarquez" $ flavor $ setTitle "title" >> p.green "act2"
              locations <- select $ LocationWithCardsUnderneath AnyCards
              chooseTargetM iid locations \lid -> do
                cards <- field LocationCardsUnderneath lid
                focusCards cards do
                  let (treacheries, rest) = partition (`cardMatch` card_ #treachery) cards
                  addToEncounterDiscard treacheries
                  placeUnderneath lid rest
        Psi -> do
          entry "larvalTunnel"
          loc <- selectJust $ locationIs Locations.larvalTunnel
          crystalRemains <-
            take 1
              <$> getSetAsideCardsMatching
                ( mapOneOf
                    cardIs
                    [Assets.crystalRemainsTheChild, Assets.crystalRemainsTheFather, Assets.crystalRemainsTheMother]
                )
          topTwo <- take 2 . unDeck <$> getEncounterDeck
          for_ topTwo obtainCard
          shuffled <- shuffle $ crystalRemains <> map toCard topTwo
          facedown <- traverse (setFacedown True) shuffled
          placeUnderneath loc facedown
        Omega -> do
          entry "saltChamber"
          loc <- selectJust $ locationIs Locations.saltChamber
          crystalRemains <-
            take 1
              <$> getSetAsideCardsMatching
                ( mapOneOf
                    cardIs
                    [Assets.crystalRemainsTheChild, Assets.crystalRemainsTheFather, Assets.crystalRemainsTheMother]
                )
          topTwo <- take 2 . unDeck <$> getEncounterDeck
          for_ topTwo obtainCard
          shuffled <- shuffle $ crystalRemains <> map toCard topTwo
          facedown <- traverse (setFacedown True) shuffled
          placeUnderneath loc facedown
        Phi -> do
          entry "crystalNursery"
          loc <- selectJust $ locationIs Locations.crystalNursery
          crystalRemains <-
            take 1
              <$> getSetAsideCardsMatching
                ( mapOneOf
                    cardIs
                    [Assets.crystalRemainsTheChild, Assets.crystalRemainsTheFather, Assets.crystalRemainsTheMother]
                )
          topTwo <- take 2 . unDeck <$> getEncounterDeck
          for_ topTwo obtainCard
          shuffled <- shuffle $ crystalRemains <> map toCard topTwo
          facedown <- traverse (setFacedown True) shuffled
          placeUnderneath loc facedown
        Sigma -> do
          entry "broodQueen"
          createSetAsideEnemy_ Enemies.broodQueenDyingMother
            $ NearestLocationToMost Anywhere
            <> FirstLocation [LocationWithTrait Cave, Anywhere]
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> scope "noResolution" do
          crystalRemainsCount <-
            selectCount $ VictoryDisplayCardMatch $ basic $ CardWithTitle "Crystal Remains"
          resolutionFlavor do
            setTitle "title"
            p "body"
            ul do
              li.validate (crystalRemainsCount == 3) "proceedToResolution1"
              li.validate (crystalRemainsCount `elem` [1, 2]) "skipToResolution2"
              li.validate (crystalRemainsCount == 0) "skipToResolution4"

          if
            | crystalRemainsCount == 3 -> push R1
            | crystalRemainsCount `elem` [1, 2] -> push R2
            | otherwise -> push R4
        Resolution 1 -> do
          record TheInvestigatorsLaidThePearlFamilyToRest
          resolution "resolution1"
          push R3
        Resolution 2 -> do
          record TheRemainsWerePartiallyRecovered
          resolution "resolution2"
          push R3
        Resolution 3 -> do
          record MadamePearlsDiaryWasRecovered
          resolution "resolution3"
          push R4
        Resolution 4 -> do
          resolutionWithXp "resolution4" $ allGainXp' attrs
          record $ AreasSurveyed PearlRidge
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheSilentHeath <$> liftRunMessage msg attrs
