module Arkham.Scenario.Scenarios.TheSecretName
  ( TheSecretName(..)
  , theSecretName
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.ClassSymbol
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding ( RevealLocation )
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheSecretName.Story
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheSecretName = TheSecretName ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretName :: Difficulty -> TheSecretName
theSecretName difficulty = scenario
  TheSecretName
  "05120"
  "The Secret Name"
  difficulty
  [ ".              .                 .             unknownPlaces4           unknownPlaces1   unknownPlaces2         unknownPlaces3 ."
  , ".              walterGilmansRoom .             cityOfElderThings        physicsClassroom siteOfTheSacrifice     .              strangeGeometry1"
  , "decrepitDoor1  moldyHalls        decrepitDoor2 moldyHallsEarlierTonight keziahsRoom      witchHouseRuins        .              ."
  , ".              decrepitDoor3     .             salemGaol1692            twilightAbyss    courtOfTheGreatOldOnes .              strangeGeometry2"
  , ".              .                 .             unknownPlaces8           unknownPlaces5   unknownPlaces6         unknownPlaces7 ."
  ]

instance HasTokenValue TheSecretName where
  getTokenValue iid tokenFace (TheSecretName attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 3 5
    Cultist -> pure $ TokenValue Cultist NoModifier
    Tablet -> pure $ TokenValue Tablet NoModifier
    ElderThing -> pure $ TokenValue ElderThing NoModifier
    otherFace -> getTokenValue iid otherFace attrs

standaloneTokens :: [TokenFace]
standaloneTokens =
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
  runMessage msg s@(TheSecretName attrs) = case msg of
    PreScenarioSetup -> do
      iids <- getInvestigatorIds
      lead <- getLead
      anyMystic <- selectAny $ InvestigatorWithClass Mystic
      membersOfTheLodge <- getHasRecord TheInvestigatorsAreMembersOfTheLodge
      enemiesOfTheLodge <- getHasRecord TheInvestigatorsAreEnemiesOfTheLodge
      learnedNothing <- getHasRecord
        TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
      neverSeenOrHeardFromAgain <- getHasRecord
        TheInvestigatorsAreNeverSeenOrHeardFromAgain
      pushAll
        $ [ storyWithChooseOne
              lead
              iids
              intro1
              [ Label
                "Tell the Lodge of the witches in the woods."
                [ story iids intro2
                , Record TheInvestigatorsToldTheLodgeAboutTheCoven
                , AddToken Cultist
                ]
              , Label
                "Tell him you know of no possible connection. (You are lying.)"
                [ story
                  iids
                  (intro3
                  <> (if anyMystic then intro3Mystic else mempty)
                  <> intro3Part2
                  )
                , Record TheInvestigatorsHidTheirKnowledgeOfTheCoven
                ]
              ]
          | membersOfTheLodge
          ]
        <> [ story iids intro4 | enemiesOfTheLodge ]
        <> [ story iids intro5 | learnedNothing ]
        <> [ story iids intro6 | neverSeenOrHeardFromAgain ]
      pure s
    SetTokensForScenario -> do
      whenM getIsStandalone $ push (SetTokens standaloneTokens)
      pure s
    Setup -> do
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.nahab, Treacheries.ghostlyPresence, Locations.strangeGeometry]
        [ EncounterSet.TheSecretName
        , EncounterSet.CityOfSins
        , EncounterSet.InexorableFate
        , EncounterSet.RealmOfDeath
        , EncounterSet.Witchcraft
        , EncounterSet.Rats
        ]

      unknownPlaces <- shuffleM =<< genCards
        [ Locations.moldyHallsEarlierTonight
        , Locations.twilightAbyss
        , Locations.cityOfElderThings
        , Locations.salemGaol1692
        , Locations.physicsClassroom
        , Locations.courtOfTheGreatOldOnes
        ]

      (moldyHallsId, placeMoldyHalls) <- placeLocationCard Locations.moldyHalls
      placeWalterGilmansRoom <- placeLocationCard_ Locations.walterGilmansRoom

      decrepitDoors <- withIndex <$> shuffleM
        [ Locations.landlordsQuarters
        , Locations.joeMazurewiczsRoom
        , Locations.frankElwoodsRoom
        ]

      decrepitDoorPlacements <- for decrepitDoors \(idx, decrepitDoor) -> do
        (locationId, placement) <- placeLocationCard decrepitDoor
        pure [placement, SetLocationLabel locationId $ "decrepitDoor" <> tshow (idx + 1)]

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

      setAsideCards <- genCards
        [ Enemies.nahab
        , Locations.siteOfTheSacrifice
        , Locations.keziahsRoom
        , Assets.theBlackBook
        , Locations.strangeGeometry
        , Locations.strangeGeometry
        , Treacheries.ghostlyPresence
        , Treacheries.ghostlyPresence
        ]

      agendas <- genCards
        [ Agendas.theHermitIX
        , Agendas.theFamiliar
        , Agendas.theWitchLight
        , Agendas.markedForSacrifice
        ]
      acts <- genCards
        [ Acts.investigatingTheWitchHouse
        , Acts.beyondTheWitchHouse
        , Acts.stoppingTheRitual
        ]

      TheSecretName <$> runMessage
        msg
        (attrs
        & (decksL . at UnknownPlacesDeck ?~ unknownPlacesDeck)
        & (setAsideCardsL .~ setAsideCards)
        & (actStackL . at 1 ?~ acts)
        & (agendaStackL . at 1 ?~ agendas)
        )
    _ -> TheSecretName <$> runMessage msg attrs
