module Arkham.Scenario.Scenarios.BloodOnTheAltar (
  BloodOnTheAltar (..),
  bloodOnTheAltar,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes hiding (matches)
import Arkham.Classes.HasGame
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (PlaceUnderneath, RevealLocation)
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BloodOnTheAltar.Story
import Arkham.Token

newtype BloodOnTheAltarMetadata = BloodOnTheAltarMetadata {sacrifices :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BloodOnTheAltar = BloodOnTheAltar (ScenarioAttrs `With` BloodOnTheAltarMetadata)
  deriving stock (Generic)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

bloodOnTheAltar :: Difficulty -> BloodOnTheAltar
bloodOnTheAltar difficulty =
  scenario
    (BloodOnTheAltar . (`with` BloodOnTheAltarMetadata []))
    "02195"
    "Blood on the Altar"
    difficulty
    [ ". houseInTheReedsHiddenChamber houseInTheReedsHiddenChamber houseInTheReeds houseInTheReeds schoolhouse schoolhouse schoolhouseHiddenChamber schoolhouseHiddenChamber ."
    , "congregationalChurchHiddenChamber congregationalChurchHiddenChamber congregationalChurch congregationalChurch villageCommons  villageCommons osbornsGeneralStore osbornsGeneralStore osbornsGeneralStoreHiddenChamber osbornsGeneralStoreHiddenChamber"
    , ". burnedRuinsHiddenChamber burnedRuinsHiddenChamber burnedRuins burnedRuins bishopsBrook bishopsBrook bishopsBrookHiddenChamber bishopsBrookHiddenChamber ."
    ]

instance HasChaosTokenValue BloodOnTheAltar where
  getChaosTokenValue iid chaosTokenFace (BloodOnTheAltar (attrs `With` _)) =
    case chaosTokenFace of
      Skull -> do
        numLocations <-
          countM (fieldMap LocationCardsUnderneath null)
            =<< select Anywhere
        pure $ toChaosTokenValue attrs Skull (min 4 numLocations) numLocations
      Cultist -> pure $ toChaosTokenValue attrs Cultist 2 4
      Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 3
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

getRemoveNecronomicon :: HasGame m => m [Message]
getRemoveNecronomicon = do
  defeatedInvestigatorIds <- select DefeatedInvestigator
  mNecronomiconOwner <- getOwner Assets.theNecronomiconOlausWormiusTranslation
  pure
    [ RemoveCampaignCard Assets.theNecronomiconOlausWormiusTranslation
    | owner <- maybeToList mNecronomiconOwner
    , owner `elem` defeatedInvestigatorIds
    ]

instance RunMessage BloodOnTheAltar where
  runMessage msg s@(BloodOnTheAltar (attrs@ScenarioAttrs {..} `With` metadata@(BloodOnTheAltarMetadata sacrificed))) =
    case msg of
      SetChaosTokensForScenario -> do
        whenM getIsStandalone $ push $ SetChaosTokens standaloneChaosTokens
        pure s
      Setup -> do
        players <- allPlayers
        bishopsBrook <-
          genCard
            =<< sample
              (Locations.bishopsBrook_202 :| [Locations.bishopsBrook_203])
        burnedRuins <-
          genCard
            =<< sample
              (Locations.burnedRuins_204 :| [Locations.burnedRuins_205])
        osbornsGeneralStore <-
          genCard
            =<< sample
              ( Locations.osbornsGeneralStore_206
                  :| [Locations.osbornsGeneralStore_207]
              )
        congregationalChurch <-
          genCard
            =<< sample
              ( Locations.congregationalChurch_208
                  :| [Locations.congregationalChurch_209]
              )
        houseInTheReeds <-
          genCard
            =<< sample
              (Locations.houseInTheReeds_210 :| [Locations.houseInTheReeds_211])
        schoolhouse <-
          genCard
            =<< sample
              (Locations.schoolhouse_212 :| [Locations.schoolhouse_213])

        oBannionGangHasABoneToPick <-
          getHasRecordOrStandalone
            OBannionGangHasABoneToPickWithTheInvestigators
            False

        (encounterCardsToPutUnderneath, encounterDeck) <-
          draw 3
            <$> buildEncounterDeckExcluding
              [ Enemies.silasBishop
              , Locations.theHiddenChamber
              , Assets.keyToTheChamber
              ]
              ( [ EncounterSet.BloodOnTheAltar
                , EncounterSet.Dunwich
                , EncounterSet.Whippoorwills
                , EncounterSet.Nightgaunts
                , EncounterSet.AncientEvils
                ]
                  <> [EncounterSet.NaomisCrew | oBannionGangHasABoneToPick]
              )

        theHiddenChamber <- genCard Locations.theHiddenChamber
        keyToTheChamber <- genCard Assets.keyToTheChamber

        cardsToPutUnderneath <-
          shuffleM
            $ keyToTheChamber
            : theHiddenChamber
            : map EncounterCard encounterCardsToPutUnderneath

        professorWarrenRiceKidnapped <-
          getHasRecordOrStandalone
            ProfessorWarrenRiceWasKidnapped
            True
        drFrancisMorganKidnapped <-
          getHasRecordOrStandalone
            DrFrancisMorganWasKidnapped
            True
        drHenryArmitageKidnapped <-
          getHasRecordOrStandalone
            DrHenryArmitageWasKidnapped
            True

        professorWarrenRice <- runMaybeT $ do
          guard professorWarrenRiceKidnapped
          lift $ genCard Assets.professorWarrenRice
        drFrancisMorgan <- runMaybeT $ do
          guard drFrancisMorganKidnapped
          lift $ genCard Assets.drFrancisMorgan
        drHenryArmitage <- runMaybeT $ do
          guard drHenryArmitageKidnapped
          lift $ genCard Assets.drHenryArmitage
        zebulonWhateley <- genCard Assets.zebulonWhateley
        earlSawyer <- genCard Assets.earlSawyer

        delayedOnTheirWayToDunwich <-
          getHasRecordOrStandalone
            TheInvestigatorsWereDelayedOnTheirWayToDunwich
            False

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

        villageCommons <- genCard Locations.villageCommons

        let
          potentialSacrifices =
            [zebulonWhateley, earlSawyer]
              <> catMaybes
                [professorWarrenRice, drFrancisMorgan, drHenryArmitage]

        (villageCommonsId, placeVillageCommons) <- placeLocation villageCommons

        otherPlacements <-
          for (zip locations cardsToPutUnderneath) $ \(location, card) -> do
            (locationId, placement) <- placeLocation location
            pure [placement, PlaceUnderneath (LocationTarget locationId) [card]]

        pushAll
          $ [ story players intro
            , SetEncounterDeck encounterDeck
            , SetAgendaDeck
            ]
          <> [placeDoomOnAgenda | delayedOnTheirWayToDunwich]
          <> [SetActDeck]
          <> (placeVillageCommons : concat otherPlacements)
          <> [ RevealLocation Nothing villageCommonsId
             , MoveAllTo (toSource attrs) villageCommonsId
             ]

        setAsideCards <-
          genCards
            [ Enemies.silasBishop
            , Locations.theHiddenChamber
            , Assets.keyToTheChamber
            , Assets.powderOfIbnGhazi
            ]
        agendas <-
          genCards
            [ Agendas.strangeDisappearances
            , Agendas.theOldOnesHunger
            , Agendas.feedTheBeast
            ]
        acts <- genCards [Acts.searchingForAnswers, Acts.theChamberOfTheBeast]

        BloodOnTheAltar
          . (`with` metadata)
          <$> runMessage
            msg
            ( attrs
                & (setAsideCardsL .~ setAsideCards)
                & (decksL . at PotentialSacrifices ?~ potentialSacrifices)
                & (actStackL . at 1 ?~ acts)
                & (agendaStackL . at 1 ?~ agendas)
            )
      ResolveChaosToken _ Tablet iid -> do
        lid <- getJustLocation iid
        matches <- (== "Hidden Chamber") . nameTitle <$> field LocationName lid
        when
          (isHardExpert attrs || (isEasyStandard attrs && matches))
          (push $ DrawAnotherChaosToken iid)
        pure s
      ResolveChaosToken _ ElderThing _ | isHardExpert attrs -> do
        agendaId <- selectJust AnyAgenda
        push $ PlaceTokens (toSource attrs) (toTarget agendaId) Doom 1
        pure s
      FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ ->
        s <$ case chaosTokenFace token of
          Cultist -> do
            lid <- getJustLocation iid
            push $ PlaceTokens (toSource attrs) (toTarget lid) Clue 1
          ElderThing | isEasyStandard attrs -> do
            agendaId <- selectJust AnyAgenda
            push $ PlaceTokens (toSource attrs) (toTarget agendaId) Doom 1
          _ -> pure ()
      ScenarioResolution NoResolution -> do
        players <- allPlayers
        agendaId <- selectJust AnyAgenda
        xp <- getXp
        let
          potentialSacrifices =
            case lookup PotentialSacrifices scenarioDecks of
              Just xs -> xs
              _ -> error "missing deck"
          sacrificedToYogSothoth = potentialSacrifices <> sacrificed
        removeNecronomicon <- getRemoveNecronomicon
        pushAll
          $ [ story players noResolution
            , Record TheRitualWasCompleted
            , PlaceUnderneath (toTarget agendaId) potentialSacrifices
            ]
          <> map (RemoveCampaignCard . toCardDef) sacrificedToYogSothoth
          <> removeNecronomicon
          <> [GainXP iid (toSource attrs) (n + 2) | (iid, n) <- xp]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 1) -> do
        players <- allPlayers
        xp <- getXp
        removeNecronomicon <- getRemoveNecronomicon
        pushAll
          $ [ story players resolution1
            , Record TheInvestigatorsPutSilasBishopOutOfHisMisery
            ]
          <> map (RemoveCampaignCard . toCardDef) sacrificed
          <> removeNecronomicon
          <> [GainXP iid (toSource attrs) (n + 2) | (iid, n) <- xp]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 2) -> do
        players <- allPlayers
        xp <- getXp
        pushAll
          $ [story players resolution2, Record TheInvestigatorsRestoredSilasBishop]
          <> map (RemoveCampaignCard . toCardDef) sacrificed
          <> [GainXP iid (toSource attrs) (n + 2) | (iid, n) <- xp]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 3) -> do
        players <- allPlayers
        xp <- getXp
        removeNecronomicon <- getRemoveNecronomicon
        pushAll
          $ [story players resolution3, Record TheInvestigatorsBanishedSilasBishop]
          <> map (RemoveCampaignCard . toCardDef) sacrificed
          <> [recordSetInsert SacrificedToYogSothoth $ map toCardCode sacrificed]
          <> removeNecronomicon
          <> [GainXP iid (toSource attrs) (n + 2) | (iid, n) <- xp]
          <> [EndOfGame Nothing]
        pure s
      _ -> BloodOnTheAltar . (`with` metadata) <$> runMessage msg attrs
