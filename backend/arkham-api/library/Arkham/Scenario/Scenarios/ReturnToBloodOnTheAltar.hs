module Arkham.Scenario.Scenarios.ReturnToBloodOnTheAltar (returnToBloodOnTheAltar) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query (getLead)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers (getHasRecordOrStandalone)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Scenarios.BloodOnTheAltar
import Arkham.Scenarios.BloodOnTheAltar.Helpers

newtype ReturnToBloodOnTheAltar = ReturnToBloodOnTheAltar BloodOnTheAltar
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasChaosTokenValue)

returnToBloodOnTheAltar :: Difficulty -> ReturnToBloodOnTheAltar
returnToBloodOnTheAltar difficulty =
  scenario
    (ReturnToBloodOnTheAltar . BloodOnTheAltar)
    "51032"
    "Return to Blood on the Altar"
    difficulty
    [ ". houseInTheReedsHiddenChamber houseInTheReedsHiddenChamber houseInTheReeds houseInTheReeds schoolhouse schoolhouse schoolhouseHiddenChamber schoolhouseHiddenChamber ."
    , "congregationalChurchHiddenChamber congregationalChurchHiddenChamber congregationalChurch congregationalChurch villageCommons  villageCommons osbornsGeneralStore osbornsGeneralStore osbornsGeneralStoreHiddenChamber osbornsGeneralStoreHiddenChamber"
    , ". burnedRuinsHiddenChamber burnedRuinsHiddenChamber burnedRuins burnedRuins bishopsBrook bishopsBrook bishopsBrookHiddenChamber bishopsBrookHiddenChamber ."
    ]

instance RunMessage ReturnToBloodOnTheAltar where
  runMessage msg s@(ReturnToBloodOnTheAltar bloodOnTheAltar'@(BloodOnTheAltar attrs)) = runQueueT $ scenarioI18n $ case msg of
    Setup -> runScenarioSetup (ReturnToBloodOnTheAltar . BloodOnTheAltar) attrs do
      oBannionGangHasABoneToPick <-
        getHasRecordOrStandalone
          OBannionGangHasABoneToPickWithTheInvestigators
          False
      setup do
        ul do
          li "gatherSets"
          li "placeLocations"
          li "placeRandomLocations"
          li "potentialSacrifices"
          li "setAside"
          li "delayed"
          unscoped $ li "shuffleRemainder"
          li.validate oBannionGangHasABoneToPick "seekingVengeance"
          li "placeUnderneath"

      scope "hiddenChamber" $ flavor do
        setTitle "title"
        p "body"

      gather Set.ReturnToBloodOnTheAltar
      gather Set.BloodOnTheAltar
      gather Set.Dunwich
      gather Set.Whippoorwills
      gather Set.Nightgaunts
      gather Set.ResurgentEvils

      bishopsBrook <-
        sample $ Locations.returnToBishopsBrook :| [Locations.bishopsBrook_202, Locations.bishopsBrook_203]
      burnedRuins <-
        sample $ Locations.returnToBurnedRuins :| [Locations.burnedRuins_204, Locations.burnedRuins_205]
      osbornsGeneralStore <-
        sample
          $ Locations.returnToOsbornsGeneralStore
          :| [Locations.osbornsGeneralStore_206, Locations.osbornsGeneralStore_207]
      congregationalChurch <-
        sample
          $ Locations.returnToCongregationalChurch
          :| [Locations.congregationalChurch_208, Locations.congregationalChurch_209]
      houseInTheReeds <-
        sample
          $ Locations.returnToHouseInTheReeds
          :| [Locations.houseInTheReeds_210, Locations.houseInTheReeds_211]
      schoolhouse <-
        sample $ Locations.returnToSchoolhouse :| [Locations.schoolhouse_212, Locations.schoolhouse_213]

      -- we set aside the Hidden Chamber and Key to the Chamber to avoid them being duplicated when we sample below
      setAside
        [ Enemies.silasBishop
        , Assets.powderOfIbnGhazi
        , Locations.theHiddenChamber
        , Assets.keyToTheChamber
        , Enemies.hiredGun
        , Enemies.hiredGun
        ]

      encounterCardsToPutUnderneath <- map toCard <$> sampleEncounterDeck 3

      -- Return to makes sure Naomis Crew is not included
      when oBannionGangHasABoneToPick $ gather Set.NaomisCrew

      theHiddenChamber <- fromSetAside Locations.theHiddenChamber
      keyToTheChamber <- fromSetAside Assets.keyToTheChamber

      cardsToPutUnderneath <-
        shuffleM $ keyToTheChamber : theHiddenChamber : encounterCardsToPutUnderneath

      delayedOnTheirWayToDunwich <-
        getHasRecordOrStandalone
          TheInvestigatorsWereDelayedOnTheirWayToDunwich
          False

      startAt =<< place Locations.villageCommonsSilentDecay

      locations <-
        drop 1
          <$> shuffleM
            [ bishopsBrook
            , burnedRuins
            , osbornsGeneralStore
            , congregationalChurch
            , houseInTheReeds
            , schoolhouse
            ]
      for_ (zip locations cardsToPutUnderneath) $ \(location, card) -> do
        l <- place location
        placeUnderneath l [toCard card]

      when delayedOnTheirWayToDunwich (placeDoomOnAgenda 1)

      setAgendaDeck [Agendas.strangeDisappearances, Agendas.theOldOnesHunger, Agendas.feedTheBeast]
      setActDeck [Acts.searchingForAnswers, Acts.theChamberOfTheBeast]

      professorWarrenRiceKidnapped <- getHasRecordOrStandalone ProfessorWarrenRiceWasKidnapped True
      drFrancisMorganKidnapped <- getHasRecordOrStandalone DrFrancisMorganWasKidnapped True
      drHenryArmitageKidnapped <- getHasRecordOrStandalone DrHenryArmitageWasKidnapped True

      professorWarrenRice <- runMaybeT $ do
        guard professorWarrenRiceKidnapped
        genCard Assets.professorWarrenRice
      drFrancisMorgan <- runMaybeT $ do
        guard drFrancisMorganKidnapped
        genCard Assets.drFrancisMorgan
      drHenryArmitage <- runMaybeT $ do
        guard drHenryArmitageKidnapped
        genCard Assets.drHenryArmitage
      zebulonWhateley <- genCard Assets.zebulonWhateley
      earlSawyer <- genCard Assets.earlSawyer
      addExtraDeck PotentialSacrifices
        =<< shuffle
          ([zebulonWhateley, earlSawyer] <> catMaybes [professorWarrenRice, drFrancisMorgan, drHenryArmitage])
      when oBannionGangHasABoneToPick $ doStep 2 msg
    DoStep 2 Setup -> do
      lead <- getLead
      locations <- select $ not_ $ orConnected $ location_ "Village Commons"
      chooseOneM lead do
        questionLabeled "Choose where to place Hired Gun"
        targets locations $ createEnemyAt_ Enemies.hiredGun
      pure s
    _ -> ReturnToBloodOnTheAltar <$> liftRunMessage msg bloodOnTheAltar'
