module Arkham.Scenario.Scenarios.TheMiskatonicMuseum (theMiskatonicMuseum, TheMiskatonicMuseum (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype TheMiskatonicMuseum = TheMiskatonicMuseum ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMiskatonicMuseum :: Difficulty -> TheMiskatonicMuseum
theMiskatonicMuseum difficulty =
  scenario TheMiskatonicMuseum "02118" "The Miskatonic Museum" difficulty scenarioLayout

instance HasChaosTokenValue TheMiskatonicMuseum where
  getChaosTokenValue iid chaosTokenFace (TheMiskatonicMuseum attrs) = case chaosTokenFace of
    Skull -> do
      huntingHorrorAtYourLocation <-
        selectAny $ enemyIs Enemies.huntingHorror <> at_ (locationWithInvestigator iid)
      pure
        $ if huntingHorrorAtYourLocation
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheMiskatonicMuseum where
  runMessage msg s@(TheMiskatonicMuseum attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      armitageKidnapped <- getHasRecordOrStandalone DrHenryArmitageWasKidnapped True
      flavor $ scope "intro" do
        h "title"
        p "body"
        p.validate armitageKidnapped "proceedToPart1"
        p.validate (not armitageKidnapped) "proceedToPart2"
      doStep (if armitageKidnapped then 1 else 2) msg
      pure s
    DoStep 1 PreScenarioSetup -> do
      flavor $ scope "intro.part1" do
        h "title"
        p "body"
      pure s
    DoStep 2 PreScenarioSetup -> do
      flavor $ scope "intro.part2" do
        h "title"
        p "body"
      pure s
    StandaloneSetup -> do
      push (SetChaosTokens standaloneChaosTokens)
      pure s
    LookAtTopOfDeck iid ScenarioDeckTarget n -> do
      case fromJustNote "must be set" (lookup ExhibitDeck attrs.decks) of
        cards -> focusCards (map flipCard $ take n cards) $ continue_ iid
      pure s
    Setup -> runScenarioSetup TheMiskatonicMuseum attrs do
      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li.nested "exhibitDeck.instructions" do
            li "exhibitDeck.bottom"
            li "exhibitDeck.top"
          li "setAside"
          unscoped $ li "shuffleRemainder"
      scope "theVoid" $ flavor do
        setTitle "title"
        p "body"

      gather Set.TheMiskatonicMuseum
      gather Set.BadLuck
      gather Set.Sorcery
      gather Set.TheBeyond
      gather Set.ChillingCold
      gather Set.LockedDoors

      setAgendaDeck [Agendas.restrictedAccess, Agendas.shadowsDeepen, Agendas.inEveryShadow]
      setActDeck
        [ Acts.findingAWayInside
        , Acts.nightAtTheMuseum
        , Acts.breakingAndEntering
        , Acts.searchingForTheTome
        ]

      startAt =<< place Locations.museumEntrance

      securityOffice <- sample2 Locations.securityOffice_128 Locations.securityOffice_129
      administrationOffice <-
        sample2 Locations.administrationOffice_130 Locations.administrationOffice_131
      placeAll [Locations.museumHalls, securityOffice, administrationOffice]

      setAside
        [ Assets.haroldWalsted
        , Assets.adamLynch
        , Assets.theNecronomiconOlausWormiusTranslation
        , Treacheries.shadowSpawned
        ]

      (bottom, top) <-
        fmap (splitAt 2)
          . shuffle
          =<< traverse fetchCard
            [ Locations.exhibitHallAthabaskanExhibit
            , Locations.exhibitHallMedusaExhibit
            , Locations.exhibitHallNatureExhibit
            , Locations.exhibitHallEgyptianExhibit
            , Locations.exhibitHallHallOfTheDead
            ]
      restrictedHall <- fetchCard Locations.exhibitHallRestrictedHall
      bottom' <- shuffle $ restrictedHall : bottom
      addExtraDeck ExhibitDeck $ top <> bottom'
    PlacedLocation name _ lid -> do
      when (nameTitle name == "Exhibit Hall") $ do
        hallCount <- selectCount $ LocationWithTitle "Exhibit Hall"
        push (SetLocationLabel lid $ "hall" <> tshow hallCount)
      pure s
    ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
      placeCluesOnLocation iid Tablet 1
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      getHuntingHorrorWith (at_ $ locationWithInvestigator iid) >>= traverse_ \huntingHorror -> do
        initiateEnemyAttack huntingHorror Tablet iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> findEncounterCardIn iid attrs (cardIs Enemies.huntingHorror) [#deck, #discard, #void]
        ElderThing -> chooseAndDiscardAsset iid ElderThing
        _ -> pure ()
      pure s
    FoundEncounterCard iid (isTarget attrs -> True) ec -> do
      withLocationOf iid $ spawnEnemyAt_ ec
      pure s
    FoundEnemyInOutOfPlay VoidZone iid (isTarget attrs -> True) eid -> do
      withLocationOf iid \lid -> push (EnemySpawnFromOutOfPlay VoidZone Nothing lid eid)
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          resolutionWithXp "noResolution" $ allGainXp' attrs
          record TheInvestigatorsFailedToRecoverTheNecronomicon
        Resolution 1 -> do
          resolutionWithXp "resolution1" $ allGainXp' attrs
          record TheInvestigatorsDestroyedTheNecronomicon
        Resolution 2 -> do
          investigators <- allInvestigators
          resolutionWithXp "resolution2" $ allGainXp' attrs
          record TheInvestigatorsTookCustodyOfTheNecronomicon
          addCampaignCardToDeckChoice
            investigators
            DoNotShuffleIn
            Assets.theNecronomiconOlausWormiusTranslation
          addChaosToken ElderThing
        other -> throwIO $ UnknownResolution other
      endOfScenario
      pure s
    _ -> TheMiskatonicMuseum <$> liftRunMessage msg attrs
