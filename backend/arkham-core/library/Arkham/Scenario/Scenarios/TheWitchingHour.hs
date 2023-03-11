module Arkham.Scenario.Scenarios.TheWitchingHour
  ( TheWitchingHour(..)
  , theWitchingHour
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheWitchingHour.Story
import Arkham.Target
import Arkham.Token
import Data.HashMap.Monoidal qualified as MonoidalHashMap
import Data.HashMap.Strict qualified as HashMap

newtype TheWitchingHour = TheWitchingHour ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchingHour :: Difficulty -> TheWitchingHour
theWitchingHour difficulty = scenario
  TheWitchingHour
  "05050"
  "The Witching Hour"
  difficulty
  [ ".      .     woods1        .      . "
  , ".      .     .             .      . "
  , "woods2 .     witchesCircle .      woods3"
  , ".      .     .             .      ."
  , ".      wood4 .             woods5 ."
  ] -- lost and separated, do we label 4 zones, or do a different placement


instance HasTokenValue TheWitchingHour where
  getTokenValue iid tokenFace (TheWitchingHour attrs) = case tokenFace of
    Skull -> pure $ toTokenValue attrs Skull 1 2
    Tablet -> pure $ toTokenValue attrs Skull 1 2
    ElderThing -> pure $ toTokenValue attrs Skull 3 4
    otherFace -> getTokenValue iid otherFace attrs

instance RunMessage TheWitchingHour where
  runMessage msg s@(TheWitchingHour attrs) = case msg of
    Setup -> do
      iids <- getInvestigatorIds
      lead <- getLeadInvestigatorId

      -- The Devourer Below is only locations
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.anetteMason]
        [ EncounterSet.TheWitchingHour
        , EncounterSet.AnettesCoven
        , EncounterSet.CityOfSins
        , EncounterSet.Witchcraft
        , EncounterSet.AncientEvils
        , EncounterSet.StrikingFear
        ]

      agentsOfShubNiggurath <- map EncounterCard
        <$> gatherEncounterSet EncounterSet.AgentsOfShubNiggurath
      agentsOfAzathoth <- map EncounterCard
        <$> gatherEncounterSet EncounterSet.AgentsOfAzathoth

      witchHauntedWoods <-
        sampleN 5
        $ Locations.witchHauntedWoodsAbandonedMine
        :| [ Locations.witchHauntedWoodsCairnStones
           , Locations.witchHauntedWoodsTheLonelyTree
           , Locations.witchHauntedWoodsChildsTreeHouse
           , Locations.witchHauntedWoodsTaintedWell
           , Locations.witchHauntedWoodsHermitsHouse
           , Locations.witchHauntedWoodsOvergrownBarn
           ]

      setAsideCards <- (<> agentsOfShubNiggurath <> agentsOfAzathoth) <$> traverse
        genCard
        [ Enemies.anetteMason
        , Locations.arkhamWoodsUnhallowedGround
        , Locations.arkhamWoodsTwistingPaths
        , Locations.arkhamWoodsOldHouse
        , Locations.arkhamWoodsCliffside
        , Locations.arkhamWoodsTangledThicket
        , Locations.arkhamWoodsQuietGlade
        ]

      let
        woodsWithInvestigators = zip (cycleN 5 iids) witchHauntedWoods
        locationMap = foldMap
          (\(investigator, location) ->
            MonoidalHashMap.singleton investigator (location :| [])
          )
          woodsWithInvestigators
      startingLocations <- HashMap.fromList <$> traverse
        (\(k, v) -> (k, ) <$> sample v)
        (MonoidalHashMap.toList locationMap)

      locationPlacements <-
        concatForM woodsWithInvestigators $ \(investigator, location) -> do
          (lid, placement) <- placeLocationCard location
          let
            mMoveTo = do
              startingLocation <- lookup investigator startingLocations
              guard $ location == startingLocation
              pure $ MoveTo $ move (toSource attrs) investigator lid

          pure
            $ placement
            : PutLocationInFrontOf investigator lid
            : maybeToList mMoveTo

      pushAll
        $ [ story iids intro1
          , chooseOne
            lead
            [ Label
              "“What can I do to avoid this fate?”"
              [SetupStep (toTarget attrs) 2]
            , Label "“This is bullshit.”" [SetupStep (toTarget attrs) 3]
            ]
          , story iids intro4
          , SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          ]
        <> locationPlacements
      TheWitchingHour <$> runMessage
        msg
        (attrs
        & (setAsideCardsL .~ setAsideCards)
        & (agendaStackL
          . at 1
          ?~ [Agendas.temperanceXIV, Agendas.theNightHowls]
          )
        & (actStackL
          . at 1
          ?~ [ Acts.lostInTheWoods
             , Acts.witchHauntings
             , Acts.pathsIntoTwilight
             , Acts.aCircleUnbroken
             ]
          )
        )
    SetupStep (isTarget attrs -> True) 2 -> do
      iids <- getInvestigatorIds
      lead <- getLeadInvestigatorId
      -- collection is infinite so we only care if the lead already has either card in their deck
      doNotAdd <- fieldMap
        InvestigatorDeck
        (any ((`elem` [Assets.theTowerXVI, Assets.aceOfRods1]) . toCardDef)
        . unDeck
        )
        lead
      pushAll
        $ [ story iids intro2
          , Record YouHaveAcceptedYourFate
          , AddToken Tablet
          , AddToken Tablet
          ]
        <> (guard doNotAdd
           *> [ AddCampaignCardToDeck lead Assets.theTowerXVI
              , AddCampaignCardToDeck lead Assets.aceOfRods1
              ]
           )
      pure s
    SetupStep (isTarget attrs -> True) 3 -> do
      iids <- getInvestigatorIds
      pushAll
        [ story iids intro3
        , Record YouHaveRejectedYourFate
        , AddToken ElderThing
        , AddToken ElderThing
        ]
      pure s
    _ -> TheWitchingHour <$> runMessage msg attrs
