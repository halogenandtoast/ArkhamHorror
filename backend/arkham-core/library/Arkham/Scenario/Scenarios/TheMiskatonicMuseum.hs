module Arkham.Scenario.Scenarios.TheMiskatonicMuseum where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Name
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Scenarios.TheMiskatonicMuseum.Story
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype TheMiskatonicMuseum = TheMiskatonicMuseum ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMiskatonicMuseum :: Difficulty -> TheMiskatonicMuseum
theMiskatonicMuseum difficulty = scenario
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

instance HasTokenValue TheMiskatonicMuseum where
  getTokenValue iid tokenFace (TheMiskatonicMuseum attrs) = case tokenFace of
    Skull -> do
      huntingHorrorAtYourLocation <-
        selectAny $ enemyIs Enemies.huntingHorror <> EnemyAt
          (LocationWithInvestigator $ InvestigatorWithId iid)
      pure $ if huntingHorrorAtYourLocation
        then toTokenValue attrs Skull 3 4
        else toTokenValue attrs Skull 1 2
    Cultist -> pure $ toTokenValue attrs Cultist 1 3
    Tablet -> pure $ toTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toTokenValue attrs ElderThing 3 5
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
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
  runMessage msg s@(TheMiskatonicMuseum attrs@ScenarioAttrs {..}) = case msg of
    SetTokensForScenario -> do
      standalone <- getIsStandalone
      s <$ if standalone then push (SetTokens standaloneTokens) else pure ()
    LookAtTopOfDeck iid ScenarioDeckTarget n -> do
      case fromJustNote "must be set" (lookup ExhibitDeck scenarioDecks) of
        xs -> do
          let cards = take n xs
          pushAll
            [ FocusCards cards
            , chooseOne iid [Label "Continue" [UnfocusCards]]
            ]
      pure s
    Setup -> do
      investigatorIds <- allInvestigatorIds

      armitageKidnapped <- getHasRecordOrStandalone
        DrHenryArmitageWasKidnapped
        True

      exhibitHalls <- shuffleM =<< traverse
        genCard
        [ Locations.exhibitHallAthabaskanExhibit
        , Locations.exhibitHallMedusaExhibit
        , Locations.exhibitHallNatureExhibit
        , Locations.exhibitHallEgyptianExhibit
        , Locations.exhibitHallHallOfTheDead
        ]

      let (bottom, top) = splitAt 2 exhibitHalls

      restrictedHall <- genCard Locations.exhibitHallRestrictedHall

      bottom' <- shuffleM $ restrictedHall : bottom -- 02137 is the restricted hall

      let exhibitDeck = top <> bottom'

      encounterDeck <- buildEncounterDeckExcluding
        [Treacheries.shadowSpawned, Assets.haroldWalsted, Assets.adamLynch]
        [ EncounterSet.TheMiskatonicMuseum
        , EncounterSet.BadLuck
        , EncounterSet.Sorcery
        , EncounterSet.TheBeyond
        , EncounterSet.ChillingCold
        , EncounterSet.LockedDoors
        ]

      museumEntrance <- genCard Locations.museumEntrance
      museumHalls <- genCard Locations.museumHalls
      securityOffice <- genCard =<< sample
        (Locations.securityOffice_128 :| [Locations.securityOffice_129])
      administrationOffice <- genCard =<< sample
        (Locations.administrationOffice_130
        :| [Locations.administrationOffice_131]
        )

      pushAllEnd
        [ story investigatorIds intro1
        , story investigatorIds (intro2 armitageKidnapped)
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , PlaceLocation securityOffice
        , PlaceLocation administrationOffice
        , PlaceLocation museumEntrance
        , PlaceLocation museumHalls
        , RevealLocation Nothing $ toLocationId museumEntrance
        , MoveAllTo (toSource attrs) $ toLocationId museumEntrance
        ]

      setAsideCards <- traverse
        genCard
        [ Assets.haroldWalsted
        , Assets.adamLynch
        , Assets.theNecronomiconOlausWormiusTranslation
        , Treacheries.shadowSpawned
        ]

      TheMiskatonicMuseum <$> runMessage
        msg
        (attrs
        & (decksL . at ExhibitDeck ?~ exhibitDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL
          . at 1
          ?~ [ Acts.findingAWayInside
             , Acts.nightAtTheMuseum
             , Acts.breakingAndEntering
             , Acts.searchingForTheTome
             ]
          )
        & (agendaStackL
          . at 1
          ?~ [ Agendas.restrictedAccess
             , Agendas.shadowsDeepen
             , Agendas.inEveryShadow
             ]
          )
        )
    PlacedLocation name _ lid -> do
      s <$ if nameTitle name == "Exhibit Hall"
        then do
          hallCount <- selectCount $ LocationWithTitle "Exhibit Hall"
          push (SetLocationLabel lid $ "hall" <> tshow hallCount)
        else pure ()
    ResolveToken _ Tablet iid | isEasyStandard attrs ->
      s <$ push (InvestigatorPlaceCluesOnLocation iid 1)
    ResolveToken _ Tablet iid | isHardExpert attrs -> do
      lid <- getJustLocation iid
      mHuntingHorrorId <- getHuntingHorrorWith $ EnemyAt $ LocationWithId lid
      case mHuntingHorrorId of
        Just huntingHorrorId ->
          s <$ push (EnemyAttack iid huntingHorrorId DamageAny RegularAttack)
        Nothing -> pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ ->
      s <$ case tokenFace token of
        Cultist -> push $ FindEncounterCard
          iid
          (toTarget attrs)
          [FromEncounterDeck, FromEncounterDiscard, FromVoid]
          (cardIs Enemies.huntingHorror)
        ElderThing -> push $ ChooseAndDiscardAsset iid (TokenEffectSource ElderThing) AnyAsset
        _ -> pure ()
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getJustLocation iid
      s <$ push (SpawnEnemyAt (EncounterCard ec) lid)
    FoundEnemyInVoid iid target eid | isTarget attrs target -> do
      lid <- getJustLocation iid
      s <$ push (EnemySpawnFromVoid Nothing lid eid)
    ScenarioResolution NoResolution -> do
      iids <- allInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story iids noResolution
          , Record TheInvestigatorsFailedToRecoverTheNecronomicon
          ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 1) -> do
      iids <- allInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story iids resolution1
          , Record TheInvestigatorsDestroyedTheNecronomicon
          ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      leadInvestigatorId <- getLeadInvestigatorId
      investigatorIds <- allInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story investigatorIds resolution2
          , Record TheInvestigatorsTookCustodyOfTheNecronomicon
          , chooseOne
            leadInvestigatorId
            [ Label
              "Add The Necronomicon (Olaus Wormius Translation) to a deck"
              [ chooseOne
                  leadInvestigatorId
                  [ TargetLabel
                      (InvestigatorTarget iid)
                      [ AddCampaignCardToDeck
                          iid
                          Assets.theNecronomiconOlausWormiusTranslation
                      ]
                  | iid <- investigatorIds
                  ]
              ]
            , Label
              "Do not add The Necronomicon (Olaus Wormius Translation) to a deck"
              []
            ]
          , AddToken ElderThing
          ]
        <> [ GainXP iid n | (iid, n) <- xp ]
        <> [EndOfGame Nothing]
      pure s
    _ -> TheMiskatonicMuseum <$> runMessage msg attrs
