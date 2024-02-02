module Arkham.Scenario.Scenarios.TheSecretName (
  TheSecretName (..),
  theSecretName,
) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (EnemyDefeated, RevealLocation)
import Arkham.Message
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheSecretName.Story
import Arkham.Trait (Trait (Extradimensional))
import Arkham.Treachery.Cards qualified as Treacheries

data Metadata = Metadata {brownJenkinDefeated :: Bool, nahabDefeated :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype TheSecretName = TheSecretName (ScenarioAttrs `With` Metadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

theSecretName :: Difficulty -> TheSecretName
theSecretName difficulty =
  scenario
    (TheSecretName . (`with` Metadata False False))
    "05120"
    "The Secret Name"
    difficulty
    [ ".              .                 .             unknownPlaces4           unknownPlaces1   unknownPlaces2         unknownPlaces3 ."
    , ".              walterGilmansRoom .             cityOfElderThings        physicsClassroom siteOfTheSacrifice     .              strangeGeometry1"
    , "decrepitDoor1  moldyHalls        decrepitDoor2 moldyHallsEarlierTonight keziahsRoom      witchHouseRuins        .              ."
    , ".              decrepitDoor3     .             salemGaol1692            twilightAbyss    courtOfTheGreatOldOnes .              strangeGeometry2"
    , ".              .                 .             .                        unknownPlaces5   unknownPlaces6         unknownPlaces7 ."
    ]

instance HasChaosTokenValue TheSecretName where
  getChaosTokenValue iid chaosTokenFace (TheSecretName (attrs `With` _)) = case chaosTokenFace of
    Skull -> do
      atExtradimensionalLocation <-
        selectAny $ locationWithInvestigator iid <> LocationWithTrait Extradimensional
      pure
        $ if atExtradimensionalLocation
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheSecretName where
  runMessage msg s@(TheSecretName (attrs `With` meta)) = case msg of
    PreScenarioSetup -> do
      players <- allPlayers
      lead <- getLeadPlayer
      anyMystic <- selectAny $ InvestigatorWithClass Mystic
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
      learnedNothing <-
        getHasRecord
          TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
      neverSeenOrHeardFromAgain <-
        getHasRecord
          TheInvestigatorsAreNeverSeenOrHeardFromAgain
      pushAll
        $ [ storyWithChooseOne
            lead
            players
            intro1
            [ Label
                "Tell the Lodge of the witches in the woods."
                [ story players intro2
                , Record TheInvestigatorsToldTheLodgeAboutTheCoven
                , AddChaosToken Cultist
                ]
            , Label
                "Tell him you know of no possible connection. (You are lying.)"
                [ story
                    players
                    $ intro3
                    <> (if anyMystic then intro3Mystic else mempty)
                    <> intro3Part2
                , Record TheInvestigatorsHidTheirKnowledgeOfTheCoven
                ]
            ]
          | membersOfTheLodge
          ]
        <> [story players intro4 | enemiesOfTheLodge]
        <> [story players intro5 | learnedNothing]
        <> [story players intro6 | neverSeenOrHeardFromAgain]
      pure s
    SetChaosTokensForScenario -> do
      whenM getIsStandalone $ push (SetChaosTokens standaloneChaosTokens)
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.nahab, Treacheries.ghostlyPresence, Locations.strangeGeometry]
          [ EncounterSet.TheSecretName
          , EncounterSet.CityOfSins
          , EncounterSet.InexorableFate
          , EncounterSet.RealmOfDeath
          , EncounterSet.Witchcraft
          , EncounterSet.Rats
          ]

      unknownPlaces <-
        shuffleM
          =<< genCards
            [ Locations.moldyHallsEarlierTonight
            , Locations.twilightAbyss
            , Locations.cityOfElderThings
            , Locations.salemGaol1692
            , Locations.physicsClassroom
            , Locations.courtOfTheGreatOldOnesANotTooDistantFuture
            ]

      (moldyHallsId, placeMoldyHalls) <- placeLocationCard Locations.moldyHalls
      placeWalterGilmansRoom <- placeLocationCard_ Locations.walterGilmansRoom

      decrepitDoors <-
        withIndex
          <$> shuffleM
            [ Locations.landlordsQuarters
            , Locations.joeMazurewiczsRoom
            , Locations.frankElwoodsRoom
            ]

      decrepitDoorPlacements <- for decrepitDoors \(idx, decrepitDoor) -> do
        (locationId, placement) <- placeLocationCard decrepitDoor
        pure
          [placement, SetLocationLabel locationId $ "decrepitDoor" <> tshow (idx + 1)]

      -- Unknown Places Deck
      let (bottom, top) = splitAt 3 unknownPlaces
      witchHouseRuins <- genCard Locations.witchHouseRuins
      bottom' <- shuffleM $ witchHouseRuins : bottom
      let unknownPlacesDeck = top <> bottom'

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeMoldyHalls
          , placeWalterGilmansRoom
          ]
        <> concat decrepitDoorPlacements
        <> [ RevealLocation Nothing moldyHallsId
           , MoveAllTo (toSource attrs) moldyHallsId
           ]

      setAsideCards <-
        genCards
          [ Enemies.nahab
          , Locations.siteOfTheSacrifice
          , Locations.keziahsRoom
          , Assets.theBlackBook
          , Locations.strangeGeometry
          , Locations.strangeGeometry
          , Treacheries.ghostlyPresence
          , Treacheries.ghostlyPresence
          ]

      agendas <-
        genCards
          [ Agendas.theHermitIX
          , Agendas.theFamiliar
          , Agendas.theWitchLight
          , Agendas.markedForSacrifice
          ]
      acts <-
        genCards
          [ Acts.investigatingTheWitchHouse
          , Acts.beyondTheWitchHouse
          , Acts.stoppingTheRitual
          ]

      TheSecretName
        . (`with` meta)
        <$> runMessage
          msg
          ( attrs
              & (decksL . at UnknownPlacesDeck ?~ unknownPlacesDeck)
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing _ | isHardExpert attrs -> do
      push HuntersMove
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist ->
          push
            $ DiscardTopOfEncounterDeck
              iid
              (if isEasyStandard attrs then 3 else 5)
              (toSource attrs)
              Nothing
        Tablet -> do
          mNahab <- selectOne $ enemyIs Enemies.nahab
          for_ mNahab $ \nahab -> do
            if isEasyStandard attrs
              then do
                atYourLocation <- nahab <=~> EnemyAt (locationWithInvestigator iid)
                when atYourLocation $ push $ EnemyWillAttack $ enemyAttack nahab attrs iid
              else push $ EnemyWillAttack $ enemyAttack nahab attrs iid
        ElderThing | isEasyStandard attrs -> push HuntersMove
        _ -> pure ()
      pure s
    EnemyDefeated _ cardId _ _ -> do
      isBrownJenkin <- selectAny $ cardIs Enemies.brownJenkin <> CardWithId cardId
      isNahab <- selectAny $ cardIs Enemies.nahab <> CardWithId cardId
      pure
        . TheSecretName
        $ attrs
        `with` meta
          { brownJenkinDefeated = brownJenkinDefeated meta || isBrownJenkin
          , nahabDefeated = nahabDefeated meta || isNahab
          }
    ScenarioResolution resolution -> do
      iids <- allInvestigatorIds
      players <- allPlayers
      step <- getCurrentActStep
      lead <- getLeadPlayer
      let
        brownJenkinBonus = if brownJenkinDefeated meta then 1 else 0
        nahabBonus = if nahabDefeated meta then 1 else 0
        addTheBlackBook =
          chooseOne lead
            $ Label "Do not add The Black Book" []
            : [ targetLabel
                iid
                [ AddCampaignCardToDeck iid Assets.theBlackBook
                , AddChaosToken Skull
                ]
              | iid <- iids
              ]
      case resolution of
        NoResolution -> pushAll [story players noResolution, scenarioResolution 1]
        Resolution 1 -> do
          gainXp <- toGainXp attrs $ getXpWithBonus (brownJenkinBonus + nahabBonus)
          pushAll
            $ story players resolution1
            : gainXp
              <> [recordSetInsert MementosDiscovered [Gilman'sJournal] | step == 2]
              <> [recordSetInsert MementosDiscovered [Keziah'sFormulae] | step == 3]
              <> [addTheBlackBook | step >= 2]
              <> [EndOfGame Nothing]
        Resolution 2 -> do
          gainXp <- toGainXp attrs $ getXpWithBonus 2
          pushAll
            $ story players resolution2
            : gainXp
              <> [ recordSetInsert
                    MementosDiscovered
                    [ Gilman'sJournal
                    , Keziah'sFormulae
                    , WornCrucifix
                    ]
                 , addTheBlackBook
                 , EndOfGame Nothing
                 ]
        _ -> error "invalid resolution"
      pure s
    _ -> TheSecretName . (`with` meta) <$> runMessage msg attrs
