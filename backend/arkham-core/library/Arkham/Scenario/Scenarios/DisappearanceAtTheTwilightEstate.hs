module Arkham.Scenario.Scenarios.DisappearanceAtTheTwilightEstate
  ( DisappearanceAtTheTwilightEstate(..)
  , disappearanceAtTheTwilightEstate
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types ( Field (..) )
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.DamageEffect
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Deck
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.DisappearanceAtTheTwilightEstate.Story
import Arkham.Target
import Arkham.Token
import Arkham.Treachery.Cards qualified as Treacheries

newtype DisappearanceAtTheTwilightEstate = DisappearanceAtTheTwilightEstate ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disappearanceAtTheTwilightEstate
  :: Difficulty -> DisappearanceAtTheTwilightEstate
disappearanceAtTheTwilightEstate difficulty = scenario
  DisappearanceAtTheTwilightEstate
  "05043"
  "Disappearance at the Twilight Estate"
  difficulty
  [ ".             .          office         .             ."
  , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
  , ".             .          entryHall      .             ."
  ]

instance HasTokenValue DisappearanceAtTheTwilightEstate where
  getTokenValue iid tokenFace (DisappearanceAtTheTwilightEstate attrs) =
    case tokenFace of
      Skull -> pure $ toTokenValue attrs Skull 3 5
      otherFace -> getTokenValue iid otherFace attrs

instance RunMessage DisappearanceAtTheTwilightEstate where
  runMessage msg s@(DisappearanceAtTheTwilightEstate attrs) = case msg of
    Setup -> do
      -- At Death's Doorstep is only locations so we will manually gather
      encounterDeck <- buildEncounterDeckExcluding
        [Enemies.theSpectralWatcher]
        [ EncounterSet.DisappearanceAtTheTwilightEstate
        , EncounterSet.InexorableFate
        , EncounterSet.RealmOfDeath
        , EncounterSet.SpectralPredators
        , EncounterSet.TrappedSpirits
        , EncounterSet.TheWatcher
        , EncounterSet.ChillingCold
        ]

      (officeId, placeOffice) <- placeLocationCard Locations.officeSpectral
      (billiardsRoomId, placeBilliardsRoom) <- placeLocationCard
        Locations.billiardsRoomSpectral
      placeTrophyRoom <- placeLocationCard_ Locations.trophyRoomSpectral
      (victorianHallsId, placeVictorianHalls) <- placeLocationCard
        Locations.victorianHallsSpectral
      placeMasterBedroom <- placeLocationCard_ Locations.masterBedroomSpectral
      (balconyId, placeBalcony) <- placeLocationCard Locations.balconySpectral
      (entryHallId, placeEntryHall) <- placeLocationCard
        Locations.entryHallSpectral

      theSpectralWatcher <- genCard Enemies.theSpectralWatcher
      netherMist <- genCard Enemies.netherMist
      obscuringFog <- genCard Treacheries.obscuringFog
      shadowHound <- genCard Enemies.shadowHound
      wraith <- genCard Enemies.wraith

      mGavriellaMizrah <- selectOne
        $ investigatorIs Investigators.gavriellaMizrah
      mJeromeDavids <- selectOne $ investigatorIs Investigators.jeromeDavids
      mValentinoRivas <- selectOne $ investigatorIs Investigators.valentinoRivas
      mPennyWhite <- selectOne $ investigatorIs Investigators.pennyWhite

      let
        victorianHallsMoveTo =
          [ MoveTo (toSource attrs) gavriellaId victorianHallsId
          | gavriellaId <- maybeToList mGavriellaMizrah
          ]
        officeMoveTo =
          [ MoveTo (toSource attrs) jeromeId officeId
          | jeromeId <- maybeToList mJeromeDavids
          ]
        billiardsRoomMoveTo =
          [ MoveTo (toSource attrs) valentinoId billiardsRoomId
          | valentinoId <- maybeToList mValentinoRivas
          ]
        balconyMoveTo =
          [ MoveTo (toSource attrs) pennyId balconyId
          | pennyId <- maybeToList mPennyWhite
          ]

      pushAll
        $ [ SetEncounterDeck
          $ removeEachFromDeck encounterDeck
          $ [ Treacheries.fateOfAllFools | isJust mGavriellaMizrah ]
          <> [ Enemies.netherMist | isJust mJeromeDavids ]
          <> [ Treacheries.obscuringFog | isJust mJeromeDavids ]
          <> [ Enemies.shadowHound | isJust mValentinoRivas ]
          <> [ Treacheries.terrorInTheNight | isJust mValentinoRivas ]
          <> [ Enemies.wraith | isJust mPennyWhite ]
          <> [ Treacheries.whispersInTheDark | isJust mPennyWhite ]
          , SetAgendaDeck
          , SetActDeck
          , placeOffice
          , placeBilliardsRoom
          , placeTrophyRoom
          , placeVictorianHalls
          , placeMasterBedroom
          , placeBalcony
          , placeEntryHall
          , CreateEnemyAt theSpectralWatcher entryHallId Nothing
          ]
        <> victorianHallsMoveTo
        <> officeMoveTo
        <> billiardsRoomMoveTo
        <> balconyMoveTo
        <> [ AttachStoryTreacheryTo obscuringFog (LocationTarget officeId)
           | isJust mJeromeDavids
           ]
        <> [ CreateEnemyAt netherMist officeId Nothing | isJust mJeromeDavids ]
        <> [ SpawnEnemyAtEngagedWith shadowHound billiardsRoomId valentinoId
           | valentinoId <- maybeToList mValentinoRivas
           ]
        <> [ AttachStoryTreacheryTo obscuringFog (LocationTarget officeId)
           | isJust mJeromeDavids
           ]
        <> [ SpawnEnemyAtEngagedWith wraith balconyId pennyId
           | pennyId <- maybeToList mPennyWhite
           ]
        <> [SetupStep (toTarget attrs) 2]

      DisappearanceAtTheTwilightEstate <$> runMessage
        msg
        (attrs
        & (actStackL . at 1 ?~ [Acts.theDisappearance])
        & (agendaStackL . at 1 ?~ [Agendas.judgementXX])
        )
    SetupStep (isTarget attrs -> True) 2 -> do
      gavriellaChosen <- selectAny
        $ investigatorIs Investigators.gavriellaMizrah
      valentinoChosen <- selectAny $ investigatorIs Investigators.valentinoRivas
      pennyChosen <- selectAny $ investigatorIs Investigators.pennyWhite

      agendaId <- selectJust AnyAgenda
      theSpectralWatcher <- selectJust $ enemyIs Enemies.theSpectralWatcher

      terrorInTheNight <- genCard Treacheries.terrorInTheNight
      whispersInTheDark <- genCard Treacheries.whispersInTheDark

      pushAll
        $ [ EnemyDamage theSpectralWatcher $ nonAttack attrs 1
          | gavriellaChosen
          ]
        <> [ AttachStoryTreacheryTo terrorInTheNight (AgendaTarget agendaId)
           | valentinoChosen
           ]
        <> [ AttachStoryTreacheryTo whispersInTheDark (AgendaTarget agendaId)
           | pennyChosen
           ]
      pure s
    FailedSkillTest iid _ _ (TokenTarget token) _ _ -> do
      case tokenFace token of
        Skull -> do
          mAction <- getSkillTestAction
          for_ mAction $ \action ->
            when (action `elem` [Action.Fight, Action.Evade]) $
              runHauntedAbilities iid
        _ -> pure ()
      pure s
    ScenarioResolution _ -> do
      iids <- allInvestigatorIds
      clues <- getSum <$> selectAgg Sum ActClues AnyAct
      pushAll
        [ story iids noResolution
        , RecordCount PiecesOfEvidenceWereLeftBehind clues
        , EndOfGame Nothing
        ]
      pure s
    _ -> DisappearanceAtTheTwilightEstate <$> runMessage msg attrs
