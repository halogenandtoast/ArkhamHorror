module Arkham.Scenario.Scenarios.ChildrenOfBlood.RiverOfBlood (riverOfBlood) where

import Arkham.Act.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Acts
import Arkham.Agenda.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Agendas
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Assets
import Arkham.Campaigns.ChildrenOfBlood.Key
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.AgentsOfZburamoarte qualified as Enemies
import Arkham.Enemy.CardDefs.ChildrenOfBlood.PreyedUpon qualified as Enemies
import Arkham.Enemy.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Agenda
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.ChaosBag (getSealedChaosTokens)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getLead, getPlayerCount)
import Arkham.Helpers.Xp
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.CardDefs.ChildrenOfBlood.RiverOfBlood qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Name (toTitle)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ChildrenOfBlood.RiverOfBlood.Helpers
import Arkham.Story.CardDefs.ChildrenOfBlood qualified as Stories
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Dawn, Dusk, Lair))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Treacheries

newtype RiverOfBlood = RiverOfBlood ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

riverOfBlood :: Difficulty -> RiverOfBlood
riverOfBlood difficulty =
  scenario
    RiverOfBlood
    "13001"
    "River of Blood"
    difficulty
    [ ".             heart     ."
    , "square        heart     triangle"
    , "square        .         triangle"
    , "unvisitedIsle circle    ."
    , "unvisitedIsle circle    ."
    , "t             .         moon"
    , "t             hourglass moon"
    , ".             hourglass ."
    ]

instance HasChaosTokenValue RiverOfBlood where
  getChaosTokenValue iid tokenFace (RiverOfBlood attrs) = case tokenFace of
    Skull -> do
      n <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage RiverOfBlood where
  runMessage msg s@(RiverOfBlood attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup RiverOfBlood attrs do
      setup $ ul do
        li "gatherSets"
        li "gatherAdditionalSets"
        li.nested "placeLocations" do
          li "dawn"
          li "dusk"
          li "startAt"
        li.nested "waterfrontCivilians" do
          li "additionalWaterfrontCivilians"
        li.nested "juliaStern" do
          li "onTheRun"
          li "stalkingTheStreets"
          li "preyingUponArkham"
        li "setOutOfPlay"
        unscoped $ li "shuffleRemainder"
        li "lairs"
        unscoped $ li "readyToBegin"

      n <- getPlayerCount
      gather Set.RiverOfBlood
      gather Set.BloodBlight
      gather Set.BloodMoon
      gather Set.ChildrenOfBlood
      gather Set.Hunted
      gather Set.Infected
      gather Set.Misinformation
      if isEasyStandard attrs
        then do
          gather Set.PreyedUpon
          gather Set.Vermin
        else do
          gather Set.AgentsOfZburamoarte
          gather Set.Mongrels
      gather Set.DeadEnds
      gather Set.FlyingTerrors
      when (n >= 3) $ gather Set.Afflicted

      setAgendaDeck
        [Agendas.theFirstDay, Agendas.theFirstNight, Agendas.theSecondDay, Agendas.theFinalNight]
      setActDeck [Acts.locateTheLair, Acts.cornerTheSuspect]

      if isEasyStandard attrs
        then do
          removeCards =<< amongGathered (#location <> CardWithTrait Dusk)
          startAt =<< place Locations.waterStreetDawn
          mainStreet <- place Locations.mainStreetDawn
          garrisonStreet <- place Locations.garrisonStreetDawn
          enemyAt_ Enemies.waterfrontCivilian mainStreet
          when (n >= 3) $ enemyAt_ Enemies.waterfrontCivilian garrisonStreet
          placeAll
            [ Locations.riverDocksDawn
            , Locations.erwinBridgeDawn
            , Locations.unvisitedIsleDawn
            , Locations.waterfrontWarehouseDawn
            , Locations.backAlleyDawn
            ]
          setAsideEvery (cardIs Enemies.nightFeeder)
        else do
          removeCards =<< amongGathered (#location <> CardWithTrait Dawn)
          startAt =<< place Locations.waterStreetDusk
          mainStreet <- place Locations.mainStreetDusk
          garrisonStreet <- place Locations.garrisonStreetDusk
          enemyAt_ Enemies.waterfrontCivilian mainStreet
          when (n >= 3) $ enemyAt_ Enemies.waterfrontCivilian garrisonStreet
          placeAll
            [ Locations.riverDocksDusk
            , Locations.erwinBridgeDusk
            , Locations.unvisitedIsleDusk
            , Locations.waterfrontWarehouseDusk
            , Locations.backAlleyDusk
            ]
          setAsideEvery (cardIs Enemies.spawnOfZburamoarte)

      setAside
        [ case attrs.difficulty of
            Easy -> Enemies.juliaSternOnTheRun
            Standard -> Enemies.juliaSternStalkingTheStreets
            _other -> Enemies.juliaSternPreyingUponArkham
        ]
      removeCards =<< amongGathered (#enemy <> CardWithTitle "Julia Stern")
      setAside =<< amongGathered (cardIs Enemies.waterfrontCivilian)
      placeStory Stories.bloodToken
      doStep 1 Setup
    DoStep 1 Setup -> do
      xs <- map toCard . toList . take 2 <$> getEncounterDeck
      julia <- fetchCard $ case attrs.difficulty of
        Easy -> Enemies.juliaSternOnTheRun
        Standard -> Enemies.juliaSternStalkingTheStreets
        _other -> Enemies.juliaSternPreyingUponArkham
      xs' <- shuffle $ julia : xs
      traverse_ obtainCard xs'
      lairs <- select $ LocationWithTrait Lair
      for_ (zip lairs xs') \(lair, x) -> placeUnderneath lair (only x)
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        whenMatch iid (InvestigatorAt $ LocationWithTrait Lair) $ assignHorror iid Tablet 1
      pure s
    ResolveChaosToken _ ElderThing iid | isHardExpert attrs -> do
      whenAny (SealedOnInvestigator (InvestigatorWithId iid) #blood) do
        afterSkillTestQuiet $ doStep 1 msg
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ | isEasyStandard attrs -> do
      case token.face of
        Tablet -> whenMatch iid (InvestigatorAt $ LocationWithTrait Lair) do
          assignHorror iid Tablet 1
        ElderThing -> whenAny (SealedOnInvestigator (InvestigatorWithId iid) #blood) do
          afterSkillTestQuiet $ doStep 1 msg
        _ -> pure ()
      pure s
    DoStep 1 (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _) | token.face == ElderThing && isEasyStandard attrs -> do
      mtkn <- selectOne $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      for_ mtkn \tkn -> do
        unsealChaosToken tkn
        assignDamage iid ElderThing 1
      pure s
    DoStep 1 (ResolveChaosToken _ ElderThing iid) -> do
      mtkn <- selectOne $ SealedOnInvestigator (InvestigatorWithId iid) #blood
      for_ mtkn \tkn -> do
        unsealChaosToken tkn
        directDamage iid ElderThing 1
      pure s
    ScenarioSpecific "placeSnare" _ -> do
      withMatch (EnemyWithTitle "Julia Stern") $ placeTokensOn ScenarioSource Token.Snare 1
      pure s
    DoStep cost (ScenarioResolution (Resolution 1)) -> do
      lead <- getLead
      choices <- (traverse toChoice =<< allInvestigators) <&> filter ((> 0) . snd . snd)
      chooseAmounts lead ("$" <> ikey "spendExperience") (TotalAmountTarget cost) choices attrs
      pure s
    ResolveAmounts _ choices (isTarget attrs -> True) -> do
      iids <- allInvestigators
      for_ iids \iid -> do
        name <- field InvestigatorName iid
        let n = getChoiceAmount (toTitle name) choices
        when (n > 0) $ push $ SpendXP iid n
      removeChaosToken #blood
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          record InvestigatorsFailedToStopJuliaStern
          addChaosToken #blood
          eachInvestigator \iid -> addCampaignCardToDeck iid ShuffleIn Treacheries.theBloodBlight
          resolutionWithXp "noResolution" $ allGainXpWithBonus' attrs (toBonus "noResolution" 3)
          push R4
        Resolution 1 -> do
          record InvestigatorsSparedJuliaStern
          xp <- allGainXp' attrs
          hasBlood <- selectAny (chaosToken_ #blood)
          cost <- (2 *) <$> getPlayerCount
          -- the GainXP messages are still queued, so affordability has to count
          -- what each investigator is about to earn on top of what they hold
          gains <- mapFromList @(Map InvestigatorId Int) <$> getXp
          available <-
            fmap sum
              . traverse (\iid -> (+ findWithDefault 0 iid gains) <$> field InvestigatorXp iid)
              =<< allInvestigators
          resolutionFlavorWithChooseOne
            (withVars ["xp" .= xp] $ setTitle "resolution1.title" >> p "resolution1.body")
            $ popScope do
              labeledValidate' (hasBlood && available >= cost) "spendExperienceToRemoveBlood"
                $ doStep cost msg
              labeled "doNotSpendExperience" nothing
          push R4
        Resolution 2 -> do
          record InvestigatorsKilledJuliaStern
          removeChaosToken #blood
          resolutionWithXp "resolution2" $ allGainXp' attrs
          push R4
        Resolution 3 -> do
          record InvestigatorsFailedToStopJuliaStern
          resolutionWithXp "resolution3" $ allGainXpWithBonus' attrs (toBonus "resolution3" 2)
          push R4
        Resolution 4 -> do
          investigators <- allInvestigators
          addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.detectiveReynoldsInOverHisHead
          whenM (remembered TheInvestigatorsFoundASacrificialDagger) do
            addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.fangOfZburamoarte
          storyCards <- getCampaignStoryCards
          for_ investigators \iid -> do
            sealed <- selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
            let bearer = any ((== Treacheries.theBloodBlight) . toCardDef) (findWithDefault [] iid storyCards)
            when (sealed >= 3 && not bearer) $ addCampaignCardToDeck iid ShuffleIn Treacheries.theBloodBlight
          bloods <- filter ((== #blood) . (.face)) <$> getSealedChaosTokens
          for_ bloods unsealChaosToken
          resolution "resolution4"
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> RiverOfBlood <$> liftRunMessage msg attrs

{- | One amount row per investigator, capped at the experience they actually
hold, so the group can split the cost unevenly.
-}
toChoice :: HasGame m => InvestigatorId -> m (Text, (Int, Int))
toChoice iid = do
  name <- field InvestigatorName iid
  x <- field InvestigatorXp iid
  pure (toTitle name, (0, x))
