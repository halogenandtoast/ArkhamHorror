module Arkham.Scenario.Scenarios.FilmFatale (filmFatale) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.EncounterSet qualified as Set
import Arkham.Exception
import Arkham.Helpers.Agenda
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FilmFatale.Helpers
import Arkham.Trait (Trait (Dinosaur, Flaw, Madness, Monster, Saturnite))

newtype FilmFatale = FilmFatale ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

filmFatale :: Difficulty -> FilmFatale
filmFatale difficulty =
  sideStory
    FilmFatale
    "72001"
    "Film Fatale"
    difficulty
    initialLayout

instance HasChaosTokenValue FilmFatale where
  getChaosTokenValue iid tokenFace (FilmFatale attrs) = case tokenFace of
    Skull -> do
      x <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull (x + 1) (x + 2)
    Cultist -> do
      monster <- selectAny $ at_ (locationWithInvestigator iid) <> EnemyWithTrait Monster
      pure $ toChaosTokenValue attrs Cultist (if monster then 5 else 3) (if monster then 6 else 4)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
standardTokens, hardTokens :: [ChaosTokenFace]
standardTokens =
  [ PlusOne , Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusTwo , MinusThree
  , MinusFour , Skull , Skull , AutoFail , ElderSign
  ]
hardTokens =
  [ Zero , Zero , MinusOne , MinusOne , MinusOne , MinusTwo , MinusThree , MinusFour , MinusSix
  , Skull , Skull , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

instance RunMessage FilmFatale where
  runMessage msg s@(FilmFatale attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      flavor do
        h "title"
        p "body"
        unscoped $ p.right "proceedToSetup"
      pure s
    StandaloneSetup -> do
      setChaosTokens $ case attrs.difficulty of
        Easy -> standardTokens
        Standard -> standardTokens
        Hard -> hardTokens
        Expert -> hardTokens
      pure s
    Setup -> runScenarioSetup FilmFatale attrs do
      setup $ ul do
        li "gatherSets"
        li.nested "placeLocations" do
          li "beginPlay"
        li "props"
        li "setOutOfPlay"
        li "setSetsOutOfPlay"
        unscoped $ li "shuffleRemainder"
        li "note"
        li "begin"
      gather Set.FilmFatale
      for_ [Set.CosmicJourney, Set.ForgottenIsland, Set.AbominableContessa] gatherAndSetAside

      centralLot <- place Locations.centralLotQuietOnSet
      startAt centralLot
      placeAll [Locations.jungleSet, Locations.spaceSet, Locations.gothicSet]

      fromGathered
        ( cardsAre
            [ Assets.heliosTelescopeGateToTheCosmos
            , Assets.staffOfTheSerpentRelicOfThePast
            , Assets.accursedCapeShroudOfChaos
            ]
        )
        >>= shuffle
        >>= placeUnderneath centralLot

      setAside [Assets.andrePatelMadeForTheSpotlight]
      setAgendaDeck [Agendas.showbusinessAsUsual, Agendas.collidingRealities]
      setActDeck [Acts.andresRequest]
    ResolveChaosToken _ Tablet iid -> do
      whenAny (EnemyWithTrait Saturnite <> at_ (locationWithInvestigator iid))
        $ drawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        ElderThing -> do
          dinosaurs <- select $ EnemyWithTrait Dinosaur <> enemyAtLocationWith iid
          chooseTargetM iid dinosaurs \enemy -> do
            ready enemy
            initiateEnemyAttack enemy ElderThing iid
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolution "noResolution"
          eachInvestigator \iid ->
            searchCollectionForRandom iid attrs $ BasicWeaknessCard <> hasAnyTrait [Madness, Flaw]
          do_ msg
        Resolution 1 -> do
          resolution "resolution1"
          selectForMaybeM (assetIs Assets.andrePatelMadeForTheSpotlight) removeAsset
          do_ msg
        Resolution 2 -> do
          resolution "resolution2"
          push R5
        Resolution 3 -> do
          resolution "resolution3"
          push R5
        Resolution 4 -> do
          resolution "resolution4"
          push R5
        Resolution 5 -> do
          resolution "resolution5"
          do_ msg
        _ -> throw $ UnknownResolution r
      pure s
    Do (ScenarioResolution _r) -> scope "resolutions" do
      madeCallTime <- remembered TheInvestigatorsMadeTheirCallTime
      resolutionWithXp "fin"
        $ if madeCallTime then allGainXpWithBonus' attrs (toBonus "bonus.score" 1) else allGainXp' attrs

      controlled <- selectAny $ AssetControlledBy Anyone <> assetIs Assets.andrePatelMadeForTheSpotlight
      when controlled do
        investigators <- allInvestigators
        addCampaignCardToDeckChoice investigators DoNotShuffleIn Assets.andrePatelMadeForTheSpotlight

      endOfScenario
      pure s
    _ -> FilmFatale <$> liftRunMessage msg attrs
