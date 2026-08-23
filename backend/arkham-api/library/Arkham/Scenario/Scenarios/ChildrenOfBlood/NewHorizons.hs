module Arkham.Scenario.Scenarios.ChildrenOfBlood.NewHorizons (newHorizons) where

import Arkham.Act.CardDefs.ChildrenOfBlood.NewHorizons qualified as Acts
import Arkham.Agenda.CardDefs.ChildrenOfBlood.NewHorizons qualified as Agendas
import Arkham.Asset.Cards.ChildrenOfBlood qualified as Assets
import Arkham.Campaigns.ChildrenOfBlood.Key
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Act (getCurrentActStep)
import Arkham.Helpers.Campaign (getCampaignStoryCards)
import Arkham.Helpers.ChaosBag (getSealedChaosTokens)
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (allInvestigators, getLead, getPlayerCount)
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Message.Lifted.Move
import Arkham.Name (toTitle)
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ChildrenOfBlood.NewHorizons.Helpers
import Arkham.Trait (Trait (Cave, Day, Night))
import Arkham.Treachery.CardDefs.ChildrenOfBlood.Infected qualified as Treacheries
import Arkham.Treachery.CardDefs.ChildrenOfBlood.NewHorizons qualified as Treacheries

newtype NewHorizons = NewHorizons ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

newHorizons :: Difficulty -> NewHorizons
newHorizons difficulty =
  scenario
    NewHorizons
    "13031"
    "New Horizons"
    difficulty
    [ ".            star         star         triangle     triangle     ."
    , "square       square       t            t            moon         moon"
    , ".            .            hourglass    hourglass    .            ."
    , "sideChamber1 sideChamber1 sideChamber2 sideChamber2 sideChamber3 sideChamber3"
    , ".            .            diamond      diamond      .            ."
    ]

instance HasChaosTokenValue NewHorizons where
  getChaosTokenValue iid tokenFace (NewHorizons attrs) = case tokenFace of
    Skull -> do
      n <- getCurrentActStep
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> do
      sealedBlood <- hasSealedBlood iid
      pure
        $ if sealedBlood && isHardExpert attrs
          then ChaosTokenValue Tablet AutoFailModifier
          else toChaosTokenValue attrs Tablet (if sealedBlood then 5 else 3) 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage NewHorizons where
  runMessage msg s@(NewHorizons attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      addChaosToken #cultist
      storyWithChooseOneM
        ( buildFlavor $ scope "intro" do
            setTitle "title"
            p "body"
            ul do
              li "addCultist"
              li.nested "chooseAsGroup" do
                li "searchDuringTheDay"
                li "searchAfterDark"
        )
        do
          labeled' "searchDuringTheDay" $ setScenarioMeta $ object ["searchAfterDark" .= False]
          labeled' "searchAfterDark" $ setScenarioMeta $ object ["searchAfterDark" .= True]
      pure s
    ResolveChaosToken _ Cultist iid -> do
      whenM (hasSealedBlood iid) $ drawAnotherChaosToken iid
      when (isEasyStandard attrs) $ afterSkillTestQuiet $ doStep 1 msg
      pure s
    DoStep 1 (ResolveChaosToken _ Cultist iid) -> releaseBlood iid >> pure s
    ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
      whenM (hasSealedBlood iid) $ afterSkillTestQuiet $ doStep 1 msg
      pure s
    DoStep 1 (ResolveChaosToken _ Tablet iid) -> releaseBlood iid >> pure s
    ResolveChaosToken _ ElderThing _iid | isHardExpert attrs -> do
      afterSkillTestQuiet $ doStep 1 msg
      pure s
    DoStep 1 (ResolveChaosToken _ ElderThing iid) -> bleed iid >> pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _
      | token.face == ElderThing && isEasyStandard attrs -> bleed iid >> pure s
    Setup -> do
      afterDark <- getScenarioMetaKeyDefault "searchAfterDark" False
      doStep (if afterDark then 2 else 1) Setup
      pure s
    DoStep 1 Setup -> runScenarioSetup NewHorizons attrs $ scope "version1" do
      setup' $ ul do
        li "gatherSets"
        li.nested "gatherLocations" do
          li "day"
          li "shallowTunnels"
          li "darkestDepths"
        li "agendaDeck"
        li "factoryWorkers"
        li.nested "zburamoarte" do
          li "lethargicBeast"
          li "sourceOfTheBlight"
          li "progenitorOfMonsters"
        li "setAside"
        li "removeNightWatchman"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.NewHorizons
      gather Set.Afflicted
      gather Set.BloodBlight
      gather Set.Bloodthirst
      gather Set.Hunted
      gather Set.Infected
      gather Set.SanguineSecrets
      gather Set.Vermin
      gather Set.FlyingTerrors

      setAgendaDeck [Agendas.busyDay, Agendas.diggingDeeperV1]
      setActDeck [Acts.toNewHorizons, Acts.theSearchForAnswers, Acts.bringDownTheBeast]

      removeCards =<< amongGathered (#location <> CardWithTrait Night)
      placeAll [Locations.managersOfficeDay, Locations.loadingDockDay, Locations.storageDay]
      factoryFloors <- traverse place [Locations.factoryFloorWestDay, Locations.factoryFloorEastDay]
      startAtFactoryFloor attrs factoryFloors
      for_ factoryFloors $ enemyAt_ Enemies.factoryWorker

      setAsideTunnels attrs
      setAsideZburamoarte attrs
      setAside [Enemies.javierRivera]
      setAsideCommon
      removeEvery [Enemies.nightWatchman]
    DoStep 2 Setup -> runScenarioSetup NewHorizons attrs $ scope "version2" do
      setup' $ ul do
        li "gatherSets"
        li.nested "gatherLocations" do
          li "night"
          li "shallowTunnels"
          li "darkestDepths"
        li "agendaDeck"
        li.nested "zburamoarte" do
          li "lethargicBeast"
          li "sourceOfTheBlight"
          li "progenitorOfMonsters"
        li "setAside"
        li "removeJavierRivera"
        unscoped $ li "shuffleRemainder"
        unscoped $ li "readyToBegin"

      gather Set.NewHorizons
      gather Set.Afflicted
      gather Set.ChildrenOfBlood
      gather Set.Infected
      gather Set.PreyedUpon
      gather Set.SanguineSecrets
      gather Set.Stalked
      gather Set.FlyingTerrors
      gather Set.ReekingDecay

      setAgendaDeck [Agendas.quietNight, Agendas.diggingDeeperV2]
      setActDeck [Acts.toNewHorizons, Acts.theSearchForAnswers, Acts.bringDownTheBeast]

      removeCards =<< amongGathered (#location <> CardWithTrait Day)
      placeAll [Locations.managersOfficeNight, Locations.loadingDockNight, Locations.storageNight]
      factoryFloors <- traverse place [Locations.factoryFloorWestNight, Locations.factoryFloorEastNight]
      startAtFactoryFloor attrs factoryFloors

      setAsideTunnels attrs
      setAsideZburamoarte attrs
      setAside [Enemies.nightWatchman]
      setAsideCommon
      removeEvery [Enemies.javierRivera, Enemies.factoryWorker]
    DoStep cost (ScenarioResolution r) | r `elem` [Resolution 1, Resolution 2] -> do
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
          addChaosToken #blood
          resolutionWithXp "noResolution" $ allGainXp' attrs
          push R5
        Resolution 1 -> do
          record InvestigatorsDidNotCompleteTheirSearch
          record InvestigatorsLeftZburamoarteAlive
          spendExperienceToRemoveBlood attrs msg "resolution1"
          push R5
        Resolution 2 -> do
          record InvestigatorsCompletedTheirSearch
          record InvestigatorsLeftZburamoarteAlive
          spendExperienceToRemoveBlood attrs msg "resolution2"
          push R5
        Resolution 3 -> do
          record InvestigatorsCompletedTheirSearch
          record InvestigatorsDefeatedZburamoarte
          removeChaosToken #blood
          resolutionWithXp "resolution3" $ allGainXp' attrs
          push R5
        Resolution 4 -> do
          record InvestigatorsWereLeftToTheCultsMercy
          addChaosToken #blood
          addChaosToken #blood
          storyCards <- getCampaignStoryCards
          eachInvestigator \iid -> do
            let bearer = any ((== Treacheries.theBloodBlight) . toCardDef) (findWithDefault [] iid storyCards)
            unless bearer $ addCampaignCardToDeck iid ShuffleIn Treacheries.theBloodBlight
          resolutionWithXp "resolution4" $ allGainXp' attrs
          push R5
        Resolution 5 -> do
          investigators <- allInvestigators
          whenM (remembered TheInvestigatorsFoundASheetOfArcaneSymbols) do
            addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.sanguineSong
          whenM (remembered TheInvestigatorsFoundForgedPermits) do
            addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.forgedPermit
          storyCards <- getCampaignStoryCards
          for_ investigators \iid -> do
            sealed <- selectCount $ SealedOnInvestigator (InvestigatorWithId iid) #blood
            let bearer = any ((== Treacheries.theBloodBlight) . toCardDef) (findWithDefault [] iid storyCards)
            when (sealed >= 2 && not bearer) $ addCampaignCardToDeck iid ShuffleIn Treacheries.theBloodBlight
          bloods <- filter ((== #blood) . (.face)) <$> getSealedChaosTokens
          for_ bloods unsealChaosToken
          resolution "resolution5"
          endOfScenario
        other -> throwIO $ UnknownResolution other
      pure s
    _ -> NewHorizons <$> liftRunMessage msg attrs

hasSealedBlood :: HasGame m => InvestigatorId -> m Bool
hasSealedBlood iid = selectAny $ SealedOnInvestigator (InvestigatorWithId iid) #blood

releaseBlood :: ReverseQueue m => InvestigatorId -> m ()
releaseBlood iid = do
  mtkn <- selectOne $ SealedOnInvestigator (InvestigatorWithId iid) #blood
  for_ mtkn unsealChaosToken

bleed :: ReverseQueue m => InvestigatorId -> m ()
bleed iid = do
  mtkn <- selectOne $ SealedOnInvestigator (InvestigatorWithId iid) #blood
  for_ mtkn \tkn -> do
    unsealChaosToken tkn
    assignDamage iid ElderThing 1

startAtFactoryFloor :: ReverseQueue m => ScenarioAttrs -> [LocationId] -> ScenarioBuilderT m ()
startAtFactoryFloor attrs factoryFloors =
  eachInvestigator \iid -> chooseTargetM iid factoryFloors $ moveTo_ attrs iid

setAsideTunnels :: ReverseQueue m => ScenarioAttrs -> ScenarioBuilderT m ()
setAsideTunnels attrs = do
  setAside
    $ Locations.descendingTunnel
    : if isEasyStandard attrs
      then
        [ Locations.cavernEntranceShallowTunnels
        , Locations.hiddenLaboratoryShallowTunnels
        , Locations.lockedChamberShallowTunnels
        , Locations.secretChamberShallowTunnels
        ]
      else
        [ Locations.cavernEntranceDarkestDepths
        , Locations.hiddenLaboratoryDarkestDepths
        , Locations.lockedChamberDarkestDepths
        , Locations.secretChamberDarkestDepths
        ]
  removeCards =<< amongGathered (#location <> CardWithTrait Cave)

setAsideZburamoarte :: ReverseQueue m => ScenarioAttrs -> ScenarioBuilderT m ()
setAsideZburamoarte attrs = do
  setAside
    [ case attrs.difficulty of
        Easy -> Enemies.zburamoarteLethargicBeast
        Standard -> Enemies.zburamoarteTheSourceOfTheBlight
        _other -> Enemies.zburamoarteProgenitorOfMonsters
    ]
  removeCards =<< amongGathered (#enemy <> CardWithTitle "Zburamoarte")

setAsideCommon :: ReverseQueue m => ScenarioBuilderT m ()
setAsideCommon = do
  setAsideEvery $ cardIs Enemies.blightedWorker
  setAsideEvery $ cardIs Treacheries.echoingInDarkness
  setAsideEvery $ CardFromEncounterSet Set.Infected
  setAsideEvery $ CardFromEncounterSet Set.FlyingTerrors
  setAside [Assets.forgedPermit, Assets.sanguineSong]

{- | Resolutions 1 and 2 both end with the optional group spend. The XP messages
are still queued, so affordability has to count what each investigator is about
to earn on top of what they hold.
-}
spendExperienceToRemoveBlood
  :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> Message -> Scope -> m ()
spendExperienceToRemoveBlood attrs msg key = do
  xp <- allGainXp' attrs
  hasBlood <- selectAny (chaosToken_ #blood)
  cost <- (2 *) <$> getPlayerCount
  gains <- mapFromList @(Map InvestigatorId Int) <$> getXp
  available <-
    fmap sum
      . traverse (\iid -> (+ findWithDefault 0 iid gains) <$> field InvestigatorXp iid)
      =<< allInvestigators
  resolutionFlavorWithChooseOne
    (withVars ["xp" .= xp] $ setTitle (key <> ".title") >> p (key <> ".body"))
    $ popScope do
      labeledValidate' (hasBlood && available >= cost) "spendExperienceToRemoveBlood"
        $ doStep cost msg
      labeled' "doNotSpendExperience" nothing

{- | One amount row per investigator, capped at the experience they actually
hold, so the group can split the cost unevenly.
-}
toChoice :: HasGame m => InvestigatorId -> m (Text, (Int, Int))
toChoice iid = do
  name <- field InvestigatorName iid
  x <- field InvestigatorXp iid
  pure (toTitle name, (0, x))
