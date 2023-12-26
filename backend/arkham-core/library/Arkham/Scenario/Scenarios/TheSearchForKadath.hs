module Arkham.Scenario.Scenarios.TheSearchForKadath (TheSearchForKadath (..), theSearchForKadath) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
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
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Prelude
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheSearchForKadath.Helpers

newtype TheSearchForKadath = TheSearchForKadath ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSearchForKadath :: Difficulty -> TheSearchForKadath
theSearchForKadath difficulty =
  scenario
    TheSearchForKadath
    "06119"
    "The Search for Kadath"
    difficulty
    ["ulthar skaiRiver dylathLeen"]

instance HasChaosTokenValue TheSearchForKadath where
  getChaosTokenValue iid tokenFace (TheSearchForKadath attrs) = case tokenFace of
    Skull -> do
      n <- getSignsOfTheGods
      pure $ toChaosTokenValue attrs Skull n (n + 1)
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ ChaosTokenValue ElderThing (PositiveModifier $ if isEasyStandard attrs then 2 else 1)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheSearchForKadath where
  runMessage msg s@(TheSearchForKadath attrs) = case msg of
    Setup -> do
      let
        setAsideCards =
          [ Enemies.catsOfUlthar
          , Enemies.stalkingManticore
          , Enemies.theCrawlingMist
          , Enemies.hordeOfNight
          , Enemies.beingsOfIb
          , Enemies.tenebrousNightgaunt
          , Enemies.tenebrousNightgaunt
          , Enemies.corsairOfLeng
          , Enemies.corsairOfLeng
          , Enemies.priestOfAThousandMasks
          , Enemies.priestOfAThousandMasks
          , Enemies.priestOfAThousandMasks
          ]
      let excludes = setAsideCards

      encounterDeck <-
        buildEncounterDeckExcluding
          excludes
          [ EncounterSet.TheSearchForKadath
          , EncounterSet.AgentsOfNyarlathotep
          , EncounterSet.Corsairs
          , EncounterSet.Dreamlands
          , EncounterSet.WhispersOfHypnos
          , EncounterSet.Zoogs
          ]

      setAside <- genCards setAsideCards

      (ulthar, placeUlthar) <- placeLocationCard Locations.ulthar
      otherPlacements <- placeLocationCards_ [Locations.skaiRiver, Locations.dylathLeen]

      agendas <- genCards [Agendas.journeyAcrossTheDreamlands, Agendas.agentsOfTheOuterGods]
      acts <-
        genCards
          [ Acts.kingdomOfTheSkai
          , Acts.theIsleOfOriab
          , Acts.theDoomThatCameBefore
          , Acts.seekOutTheNight
          , Acts.theKingsDecree
          ]
      lead <- getLead
      virgil <- genCard Assets.virgilGray

      pushAll
        $ [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck, placeUlthar]
        <> otherPlacements
        <> [MoveAllTo (toSource attrs) ulthar]
        <> [TakeControlOfSetAsideAsset lead virgil]

      TheSearchForKadath
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAside)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> void $ runMaybeT $ do
          Action.Investigate <- MaybeT getSkillTestAction
          LocationTarget lid <- MaybeT getSkillTestTarget
          lift $ push $ roundModifier Cultist lid (ShroudModifier $ if isEasyStandard attrs then 1 else 2)
        Tablet -> do
          player <- getPlayer iid
          push
            $ chooseOne
              player
              [ Label "Take 1 damage and 1 horror" [assignDamageAndHorror iid Tablet 1 1]
              , Label "Place 1 doom on the current agenda" [PlaceDoomOnAgenda]
              ]
        _ -> pure ()
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        ElderThing -> void $ runMaybeT $ do
          Action.Investigate <- MaybeT getSkillTestAction
          lift $ push $ skillTestModifier ElderThing iid (DiscoveredClues 1)
        _ -> pure ()
      pure s
    _ -> TheSearchForKadath <$> runMessage msg attrs
