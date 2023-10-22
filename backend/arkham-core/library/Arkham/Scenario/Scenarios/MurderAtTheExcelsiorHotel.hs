module Arkham.Scenario.Scenarios.MurderAtTheExcelsiorHotel (
  MurderAtTheExcelsiorHotel (..),
  murderAtTheExcelsiorHotel,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.MurderAtTheExcelsiorHotel.FlavorText
import Arkham.Trait (Trait (Guest, Innocent))
import Arkham.Treachery.Cards qualified as Treacheries

newtype MurderAtTheExcelsiorHotel = MurderAtTheExcelsiorHotel ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

murderAtTheExcelsiorHotel :: Difficulty -> MurderAtTheExcelsiorHotel
murderAtTheExcelsiorHotel difficulty =
  scenario
    MurderAtTheExcelsiorHotel
    "84001"
    "Murder at the Excelsior Hotel"
    difficulty
    [ ".           .         hotelRoof       hotelRoof       room245  room245 .            .           "
    , "room212     room212   hotelRoof       hotelRoof       room245  room245 .            .           "
    , "room212     room212   secondFloorHall secondFloorHall room225  room225 suiteBalcony suiteBalcony"
    , "restaurant restaurant secondFloorHall secondFloorHall room225  room225 suiteBalcony suiteBalcony"
    , "restaurant restaurant foyer           foyer           office   office  .            ."
    , ".          .          foyer           foyer           office   office  .            ."
    , ".          .          .               basement        basement .       .            ."
    , ".          .          .               basement        basement .       .            ."
    ]

instance HasChaosTokenValue MurderAtTheExcelsiorHotel where
  getChaosTokenValue iid tokenFace (MurderAtTheExcelsiorHotel attrs) = case tokenFace of
    Skull -> do
      guests <- selectCount $ EnemyWithTrait Guest
      innocents <- selectCount $ EnemyWithTrait Innocent
      pure $ toChaosTokenValue attrs Skull guests innocents
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 3
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage MurderAtTheExcelsiorHotel where
  runMessage msg s@(MurderAtTheExcelsiorHotel attrs) = case msg of
    PreScenarioSetup -> do
      players <- allPlayers
      pushAll
        [ story players intro1
        , story players (if length players == 1 then intro2 else intro3)
        , story players intro4
        ]
      pure s
    Setup -> do
      lead <- getLead
      otherPlayers <- deleteFirst lead <$> allInvestigators
      encounterDeck <-
        buildEncounterDeckExcluding [Enemies.arkhamOfficer] [EncounterSet.MurderAtTheExcelsiorHotel]

      (room225, placeRoom225) <- placeLocationCard Locations.room225
      (foyer, placeFoyer) <- placeLocationCard Locations.foyerMurderAtTheExcelsiorHotel

      otherPlacements <-
        placeLocationCards_ [Locations.suiteBalcony, Locations.secondFloorHall, Locations.restaurant]

      bloodstainedDagger <- genCard Assets.bloodstainedDagger
      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetActDeck
          , SetAgendaDeck
          , placeRoom225
          , placeFoyer
          , MoveTo $ move (toSource attrs) lead room225
          , TakeControlOfSetAsideAsset lead bloodstainedDagger
          ]
        <> [MoveTo $ move (toSource attrs) other foyer | other <- otherPlayers]
        <> otherPlacements

      setAsideCards <-
        genCards
          [ Assets.sergeantMonroe
          , Treacheries.whatHaveYouDone
          , Enemies.arkhamOfficer
          , Enemies.arkhamOfficer
          , Enemies.arkhamOfficer
          ]

      agendas <- genCards [Agendas.theMurder, Agendas.specialInvestigation]
      acts <- genCards [Acts.whatHappened, Acts.followingLeads]

      leadsDeck <-
        shuffleM
          =<< genCards
            [ Assets.alienDevice
            , Assets.managersKey
            , Assets.tomeOfRituals
            , Assets.sinisterSolution
            , Assets.timeWornLocket
            ]

      MurderAtTheExcelsiorHotel
        <$> runMessage
          msg
          ( attrs
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
              & (decksL . at LeadsDeck ?~ leadsDeck)
              & (setAsideCardsL .~ setAsideCards)
          )
    SetChaosTokensForScenario -> do
      let
        tokens =
          if isEasyStandard attrs
            then
              [ PlusOne
              , Zero
              , MinusOne
              , MinusOne
              , MinusTwo
              , MinusThree
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
            else
              [ Zero
              , MinusOne
              , MinusTwo
              , MinusThree
              , MinusFour
              , MinusFour
              , MinusFive
              , MinusSix
              , Skull
              , Skull
              , Cultist
              , Tablet
              , ElderThing
              , AutoFail
              , ElderSign
              ]
      push $ SetChaosTokens tokens
      pure s
    ResolveChaosToken _ Cultist iid -> do
      innocentInVictory <- selectAny $ VictoryDisplayCardMatch $ #enemy <> withTrait Innocent
      pushWhen innocentInVictory $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken token Tablet iid -> do
      clues <- field InvestigatorClues iid
      mLocation <- field InvestigatorLocation iid
      for_ mLocation \_ ->
        when (clues > 0) do
          player <- getPlayer iid
          let n = if isEasyStandard attrs then 1 else 2
          push
            $ chooseOne
              player
              [ Label
                  ("Place one of your clues on your location to treat this as a -" <> tshow n)
                  [ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1
                  , skillTestModifier
                      (ChaosTokenEffectSource Tablet)
                      token
                      (ChangeChaosTokenModifier (NegativeModifier n))
                  ]
              , Label "Skip" []
              ]

      pure s
    ResolveChaosToken _ ElderThing iid -> do
      innocentInVictory <- selectAny $ VictoryDisplayCardMatch $ #enemy <> withTrait Innocent
      pushWhen innocentInVictory $ assignHorror iid (ChaosTokenEffectSource Tablet) 1
      pure s
    _ -> MurderAtTheExcelsiorHotel <$> runMessage msg attrs
