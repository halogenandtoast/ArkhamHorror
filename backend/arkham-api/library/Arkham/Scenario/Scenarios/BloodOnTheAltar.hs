module Arkham.Scenario.Scenarios.BloodOnTheAltar (bloodOnTheAltar, BloodOnTheAltar (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheDunwichLegacy.Key
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Card
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location
import Arkham.Helpers.Xp
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath, RevealLocation)
import Arkham.Message.Lifted.Log
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (..))
import Arkham.Scenarios.BloodOnTheAltar.Helpers
import Arkham.Token

newtype BloodOnTheAltar = BloodOnTheAltar ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

bloodOnTheAltar :: Difficulty -> BloodOnTheAltar
bloodOnTheAltar difficulty =
  scenario
    BloodOnTheAltar
    "02195"
    "Blood on the Altar"
    difficulty
    [ ". houseInTheReedsHiddenChamber houseInTheReedsHiddenChamber houseInTheReeds houseInTheReeds schoolhouse schoolhouse schoolhouseHiddenChamber schoolhouseHiddenChamber ."
    , "congregationalChurchHiddenChamber congregationalChurchHiddenChamber congregationalChurch congregationalChurch villageCommons  villageCommons osbornsGeneralStore osbornsGeneralStore osbornsGeneralStoreHiddenChamber osbornsGeneralStoreHiddenChamber"
    , ". burnedRuinsHiddenChamber burnedRuinsHiddenChamber burnedRuins burnedRuins bishopsBrook bishopsBrook bishopsBrookHiddenChamber bishopsBrookHiddenChamber ."
    ]

instance HasChaosTokenValue BloodOnTheAltar where
  getChaosTokenValue iid chaosTokenFace (BloodOnTheAltar attrs) =
    case chaosTokenFace of
      Skull -> do
        numLocations <- countM (fieldMap LocationCardsUnderneath null) =<< select Anywhere
        pure $ toChaosTokenValue attrs Skull (min 4 numLocations) numLocations
      Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
      Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 3
      otherFace -> getChaosTokenValue iid otherFace attrs

{- FOURMOLU_DISABLE -}
standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-3" , #"-4"
  , Skull , Skull , Cultist , Tablet , ElderThing , AutoFail , ElderSign
  ]
{- FOURMOLU_ENABLE -}

removeNecronomicon :: ReverseQueue m => m ()
removeNecronomicon = do
  defeatedInvestigatorIds <- select DefeatedInvestigator
  withOwner Assets.theNecronomiconOlausWormiusTranslation \owner -> do
    when (owner `elem` defeatedInvestigatorIds) do
      removeCampaignCard Assets.theNecronomiconOlausWormiusTranslation

instance RunMessage BloodOnTheAltar where
  runMessage msg s@(BloodOnTheAltar attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" do
        h "title"
        p "body"
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup BloodOnTheAltar attrs do
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

      gather Set.BloodOnTheAltar
      gather Set.Dunwich
      gather Set.Whippoorwills
      gather Set.Nightgaunts
      gather Set.AncientEvils

      when oBannionGangHasABoneToPick $ gather Set.NaomisCrew

      bishopsBrook <- sample2 Locations.bishopsBrook_202 Locations.bishopsBrook_203
      burnedRuins <- sample2 Locations.burnedRuins_204 Locations.burnedRuins_205
      osbornsGeneralStore <- sample2 Locations.osbornsGeneralStore_206 Locations.osbornsGeneralStore_207
      congregationalChurch <-
        sample2 Locations.congregationalChurch_208 Locations.congregationalChurch_209
      houseInTheReeds <- sample2 Locations.houseInTheReeds_210 Locations.houseInTheReeds_211
      schoolhouse <- sample2 Locations.schoolhouse_212 Locations.schoolhouse_213

      -- we set aside the Hidden Chamber and Key to the Chamber to avoid them being duplicated when we sample below
      setAside
        [Enemies.silasBishop, Assets.powderOfIbnGhazi, Locations.theHiddenChamber, Assets.keyToTheChamber]

      encounterCardsToPutUnderneath <- map toCard <$> sampleEncounterDeck 3

      theHiddenChamber <- fromSetAside Locations.theHiddenChamber
      keyToTheChamber <- fromSetAside Assets.keyToTheChamber

      cardsToPutUnderneath <-
        shuffleM $ keyToTheChamber : theHiddenChamber : encounterCardsToPutUnderneath

      delayedOnTheirWayToDunwich <-
        getHasRecordOrStandalone
          TheInvestigatorsWereDelayedOnTheirWayToDunwich
          False

      startAt =<< place Locations.villageCommons

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
    ResolveChaosToken _ Tablet iid -> do
      if isHardExpert attrs
        then drawAnotherChaosToken iid
        else withLocationOf iid \lid -> do
          isHiddenChamber <- (== "The Hidden Chamber") . nameTitle <$> field LocationName lid
          when isHiddenChamber $ drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing _ | isHardExpert attrs -> do
      agendaId <- selectJust AnyAgenda
      placeDoom attrs agendaId 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Cultist -> withLocationOf iid \lid ->
          push $ PlaceTokens (toSource attrs) (toTarget lid) Clue 1
        ElderThing | isEasyStandard attrs -> do
          agendaId <- selectJust AnyAgenda
          placeDoom attrs agendaId 1
        _ -> pure ()
      pure s
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          let
            potentialSacrifices =
              case lookup PotentialSacrifices attrs.decks of
                Just xs -> xs
                _ -> error "missing deck"
          agendaId <- selectJust AnyAgenda
          placeUnderneath agendaId potentialSacrifices
        _ -> pure ()
      do_ msg
      pure s
    Do (ScenarioResolution r) -> scope "resolutions" do
      sacrificed <- filterCards CardIsUnique <$> scenarioField ScenarioCardsUnderAgendaDeck
      let doGainXp = allGainXpWithBonus' attrs $ toBonus "bonus" 2
      case r of
        NoResolution -> do
          resolutionWithXp "noResolution" doGainXp
          record TheRitualWasCompleted
          removeNecronomicon
        Resolution 1 -> do
          resolutionWithXp "resolution1" doGainXp
          record TheInvestigatorsPutSilasBishopOutOfHisMisery
          removeNecronomicon
        Resolution 2 -> do
          resolutionWithXp "resolution2" doGainXp
          record TheInvestigatorsRestoredSilasBishop
        Resolution 3 -> do
          resolutionWithXp "resolution3" doGainXp
          record TheInvestigatorsBanishedSilasBishop
          removeNecronomicon
        other -> throwIO $ UnknownResolution other

      recordSetInsert SacrificedToYogSothoth $ map toCardCode sacrificed
      for_ sacrificed removeCampaignCard
      endOfScenario
      pure s
    _ -> BloodOnTheAltar <$> liftRunMessage msg attrs
