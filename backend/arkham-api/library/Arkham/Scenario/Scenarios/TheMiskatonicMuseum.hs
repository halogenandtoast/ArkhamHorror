module Arkham.Scenario.Scenarios.TheMiskatonicMuseum where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Name
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (addCampaignCardToDeckChoice)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Scenarios.TheMiskatonicMuseum.Story
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype TheMiskatonicMuseum = TheMiskatonicMuseum ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMiskatonicMuseum :: Difficulty -> TheMiskatonicMuseum
theMiskatonicMuseum difficulty =
  scenario
    TheMiskatonicMuseum
    "02118"
    "The Miskatonic Museum"
    difficulty
    [ ".     .     .                    .                    hall3 hall3          hall4          hall4 .                  .              .     ."
    , ".     .     hall2                hall2                hall3 hall3          hall4          hall4 hall5              hall5          .     ."
    , "hall1 hall1 hall2                hall2                .     museumHalls    museumHalls    .     hall5              hall5          hall6 hall6"
    , "hall1 hall1 .                    .                    .     museumHalls    museumHalls    .     .                  .              hall6 hall6"
    , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
    , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
    ]

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
  runMessage msg s@(TheMiskatonicMuseum attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro1
      armitageKidnapped <- getHasRecordOrStandalone DrHenryArmitageWasKidnapped True
      story (intro2 armitageKidnapped)
      pure s
    StandaloneSetup -> do
      push (SetChaosTokens standaloneChaosTokens)
      pure s
    LookAtTopOfDeck iid ScenarioDeckTarget n -> do
      case fromJustNote "must be set" (lookup ExhibitDeck attrs.decks) of
        cards -> focusCards (map flipCard $ take n cards) \unfocus -> continue iid [unfocus]
      pure s
    Setup -> runScenarioSetup TheMiskatonicMuseum attrs do
      gather Set.TheMiskatonicMuseum
      gather Set.BadLuck
      gather Set.Sorcery
      gather Set.TheBeyond
      gather Set.ChillingCold
      gather Set.LockedDoors

      startAt =<< place Locations.museumEntrance

      securityOffice <- sample $ Locations.securityOffice_128 :| [Locations.securityOffice_129]
      administrationOffice <-
        sample $ Locations.administrationOffice_130 :| [Locations.administrationOffice_131]
      placeAll [Locations.museumHalls, securityOffice, administrationOffice]

      setAside
        [ Assets.haroldWalsted
        , Assets.adamLynch
        , Assets.theNecronomiconOlausWormiusTranslation
        , Treacheries.shadowSpawned
        ]

      setAgendaDeck [Agendas.restrictedAccess, Agendas.shadowsDeepen, Agendas.inEveryShadow]
      setActDeck
        [ Acts.findingAWayInside
        , Acts.nightAtTheMuseum
        , Acts.breakingAndEntering
        , Acts.searchingForTheTome
        ]

      (bottom, top) <-
        fmap (splitAt 2)
          . shuffleM
          =<< genCards
            [ Locations.exhibitHallAthabaskanExhibit
            , Locations.exhibitHallMedusaExhibit
            , Locations.exhibitHallNatureExhibit
            , Locations.exhibitHallEgyptianExhibit
            , Locations.exhibitHallHallOfTheDead
            ]
      restrictedHall <- genCard Locations.exhibitHallRestrictedHall
      bottom' <- shuffleM $ restrictedHall : bottom
      addExtraDeck ExhibitDeck $ top <> bottom'
    PlacedLocation name _ lid -> do
      when (nameTitle name == "Exhibit Hall") $ do
        hallCount <- selectCount $ LocationWithTitle "Exhibit Hall"
        push (SetLocationLabel lid $ "hall" <> tshow hallCount)
      pure s
    ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
      push (InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1)
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      mHuntingHorrorId <- getHuntingHorrorWith $ at_ $ locationWithInvestigator iid
      for_ mHuntingHorrorId \huntingHorrorId ->
        push (EnemyAttack $ enemyAttack huntingHorrorId attrs iid)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist ->
          push
            $ FindEncounterCard
              iid
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard, FromVoid]
              (cardIs Enemies.huntingHorror)
        ElderThing ->
          push $ ChooseAndDiscardAsset iid (ChaosTokenEffectSource ElderThing) AnyAsset
        _ -> pure ()
      pure s
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getJustLocation iid
      push (SpawnEnemyAt (EncounterCard ec) lid)
      pure s
    FoundEnemyInOutOfPlay VoidZone iid target eid | isTarget attrs target -> do
      lid <- getJustLocation iid
      push (EnemySpawnFromOutOfPlay VoidZone Nothing lid eid)
      pure s
    ScenarioResolution NoResolution -> do
      story noResolution
      record TheInvestigatorsFailedToRecoverTheNecronomicon
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 1) -> do
      story resolution1
      record TheInvestigatorsDestroyedTheNecronomicon
      allGainXp attrs
      endOfScenario
      pure s
    ScenarioResolution (Resolution 2) -> do
      investigatorIds <- allInvestigators
      story resolution2
      record TheInvestigatorsTookCustodyOfTheNecronomicon
      addCampaignCardToDeckChoice investigatorIds Assets.theNecronomiconOlausWormiusTranslation
      addChaosToken ElderThing
      allGainXp attrs
      endOfScenario
      pure s
    _ -> TheMiskatonicMuseum <$> liftRunMessage msg attrs
