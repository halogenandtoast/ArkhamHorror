module Arkham.Scenario.Scenarios.BloodOnTheAltar
  ( BloodOnTheAltar(..)
  , bloodOnTheAltar
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Card.EncounterCard
import Arkham.Card.PlayerCard
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Card
import Arkham.Helpers.Investigator
import Arkham.Id
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types ( Field (..) )
import Arkham.Matcher hiding ( PlaceUnderneath, RevealLocation )
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding ( matches )
import Arkham.Scenario.Runner
import Arkham.Scenarios.BloodOnTheAltar.Story
import Arkham.Target
import Arkham.Token

newtype BloodOnTheAltarMetadata = BloodOnTheAltarMetadata { sacrifices :: [Card]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype BloodOnTheAltar = BloodOnTheAltar (ScenarioAttrs `With` BloodOnTheAltarMetadata)
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

bloodOnTheAltar :: Difficulty -> BloodOnTheAltar
bloodOnTheAltar difficulty = scenario
  (BloodOnTheAltar . (`with` BloodOnTheAltarMetadata []))
  "02195"
  "Blood on the Altar"
  difficulty
  [ ". houseInTheReedsHiddenChamber houseInTheReedsHiddenChamber houseInTheReeds houseInTheReeds schoolhouse schoolhouse schoolhouseHiddenChamber schoolhouseHiddenChamber ."
  , "congregationalChurchHiddenChamber congregationalChurchHiddenChamber congregationalChurch congregationalChurch villageCommons  villageCommons osbornsGeneralStore osbornsGeneralStore osbornsGeneralStoreHiddenChamber osbornsGeneralStoreHiddenChamber"
  , ". burnedRuinsHiddenChamber burnedRuinsHiddenChamber burnedRuins burnedRuins bishopsBrook bishopsBrook bishopsBrookHiddenChamber bishopsBrookHiddenChamber ."
  ]

instance HasTokenValue BloodOnTheAltar where
  getTokenValue iid tokenFace (BloodOnTheAltar (attrs `With` _)) =
    case tokenFace of
      Skull -> do
        numLocations <- countM (fieldMap LocationCardsUnderneath null)
          =<< selectList Anywhere
        pure $ toTokenValue attrs Skull (min 4 numLocations) numLocations
      Cultist -> pure $ toTokenValue attrs Cultist 2 4
      Tablet -> pure $ toTokenValue attrs Tablet 2 3
      ElderThing -> pure $ toTokenValue attrs ElderThing 3 3
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

getRemoveNecronomicon :: (Monad m, HasGame m) => m [Message]
getRemoveNecronomicon = do
  defeatedInvestigatorIds <- selectList DefeatedInvestigator
  mNecronomiconOwner <- getOwner Assets.theNecronomiconOlausWormiusTranslation
  pure
    [ RemoveCampaignCard Assets.theNecronomiconOlausWormiusTranslation
    | owner <- maybeToList mNecronomiconOwner
    , owner `elem` defeatedInvestigatorIds
    ]

instance RunMessage BloodOnTheAltar where
  runMessage msg s@(BloodOnTheAltar (attrs@ScenarioAttrs {..} `With` metadata@(BloodOnTheAltarMetadata sacrificed)))
    = case msg of
      SetTokensForScenario -> do
        whenM getIsStandalone $ push $ SetTokens standaloneTokens
        pure s
      Setup -> do
        investigatorIds <- allInvestigatorIds
        bishopsBrook <-
          genCard =<< sample
            (Locations.bishopsBrook_202 :| [Locations.bishopsBrook_203])
        burnedRuins <-
          genCard =<< sample
            (Locations.burnedRuins_204 :| [Locations.burnedRuins_205])
        osbornsGeneralStore <- genCard =<< sample
          (Locations.osbornsGeneralStore_206
          :| [Locations.osbornsGeneralStore_207]
          )
        congregationalChurch <- genCard =<< sample
          (Locations.congregationalChurch_208
          :| [Locations.congregationalChurch_209]
          )
        houseInTheReeds <- genCard =<< sample
          (Locations.houseInTheReeds_210 :| [Locations.houseInTheReeds_211])
        schoolhouse <-
          genCard =<< sample
            (Locations.schoolhouse_212 :| [Locations.schoolhouse_213])

        oBannionGangHasABoneToPick <- getHasRecordOrStandalone
          OBannionGangHasABoneToPickWithTheInvestigators
          False

        (encounterCardsToPutUnderneath, encounterDeck) <-
          splitAt 3 . unDeck <$> buildEncounterDeckExcluding
            [ Enemies.silasBishop
            , Locations.theHiddenChamber
            , Assets.keyToTheChamber
            ]
            ([ EncounterSet.BloodOnTheAltar
             , EncounterSet.Dunwich
             , EncounterSet.Whippoorwills
             , EncounterSet.Nightgaunts
             , EncounterSet.AncientEvils
             ]
            <> [ EncounterSet.NaomisCrew | oBannionGangHasABoneToPick ]
            )

        theHiddenChamber <- EncounterCard
          <$> genEncounterCard Locations.theHiddenChamber
        keyToTheChamber <- EncounterCard
          <$> genEncounterCard Assets.keyToTheChamber

        cardsToPutUnderneath <-
          shuffleM
          $ keyToTheChamber
          : theHiddenChamber
          : map EncounterCard encounterCardsToPutUnderneath

        professorWarrenRiceKidnapped <- getHasRecordOrStandalone
          ProfessorWarrenRiceWasKidnapped
          True
        drFrancisMorganKidnapped <- getHasRecordOrStandalone
          DrFrancisMorganWasKidnapped
          True
        drHenryArmitageKidnapped <- getHasRecordOrStandalone
          DrHenryArmitageWasKidnapped
          True

        professorWarrenRice <- if professorWarrenRiceKidnapped
          then Just . PlayerCard <$> genPlayerCard Assets.professorWarrenRice
          else pure Nothing
        drFrancisMorgan <- if drFrancisMorganKidnapped
          then Just . PlayerCard <$> genPlayerCard Assets.drFrancisMorgan
          else pure Nothing
        drHenryArmitage <- if drHenryArmitageKidnapped
          then Just . PlayerCard <$> genPlayerCard Assets.drHenryArmitage
          else pure Nothing
        zebulonWhateley <- PlayerCard <$> genPlayerCard Assets.zebulonWhateley
        earlSawyer <- PlayerCard <$> genPlayerCard Assets.earlSawyer

        delayedOnTheirWayToDunwich <- getHasRecordOrStandalone
          TheInvestigatorsWereDelayedOnTheirWayToDunwich
          False

        locations <- drop 1 <$> shuffleM
          [ bishopsBrook
          , burnedRuins
          , osbornsGeneralStore
          , congregationalChurch
          , houseInTheReeds
          , schoolhouse
          ]

        villageCommons <- EncounterCard
          <$> genEncounterCard Locations.villageCommons

        let
          locationCardPairs = zip locations cardsToPutUnderneath
          potentialSacrifices = [zebulonWhateley, earlSawyer] <> catMaybes
            [professorWarrenRice, drFrancisMorgan, drHenryArmitage]

        pushAll
          $ [ story investigatorIds intro
            , SetEncounterDeck (Deck encounterDeck)
            , SetAgendaDeck
            ]
          <> [ PlaceDoomOnAgenda | delayedOnTheirWayToDunwich ]
          <> [SetActDeck, PlaceLocation villageCommons]
          <> concat
               [ [ PlaceLocation location
                 , PlaceUnderneath
                   (LocationTarget $ toLocationId location)
                   [card]
                 ]
               | (location, card) <- locationCardPairs
               ]
          <> [ RevealLocation Nothing (LocationId $ toCardId villageCommons)
             , MoveAllTo (toSource attrs) (LocationId $ toCardId villageCommons)
             ]

        setAsideCards <- traverse
          genCard
          [ Enemies.silasBishop
          , Locations.theHiddenChamber
          , Assets.keyToTheChamber
          , Assets.powderOfIbnGhazi
          ]

        BloodOnTheAltar . (`with` metadata) <$> runMessage
          msg
          (attrs
          & (setAsideCardsL .~ setAsideCards)
          & (decksL . at PotentialSacrifices ?~ potentialSacrifices)
          & (actStackL
            . at 1
            ?~ [Acts.searchingForAnswers, Acts.theChamberOfTheBeast]
            )
          & (agendaStackL
            . at 1
            ?~ [ Agendas.strangeDisappearances
               , Agendas.theOldOnesHunger
               , Agendas.feedTheBeast
               ]
            )
          )
      ResolveToken _ Tablet iid -> do
        lid <- getJustLocation iid
        matches <- (== "Hidden Chamber") . nameTitle <$> field LocationName lid
        s <$ when
          (isHardExpert attrs || (isEasyStandard attrs && matches))
          (push $ DrawAnotherToken iid)
      ResolveToken _ ElderThing _ | isHardExpert attrs -> do
        agendaId <- selectJust AnyAgenda
        s <$ push (PlaceDoom (AgendaTarget agendaId) 1)
      FailedSkillTest iid _ _ (TokenTarget token) _ _ ->
        s <$ case tokenFace token of
          Cultist -> do
            lid <- getJustLocation iid
            push (PlaceClues (LocationTarget lid) 1)
          ElderThing | isEasyStandard attrs -> do
            agendaId <- selectJust AnyAgenda
            push (PlaceDoom (AgendaTarget agendaId) 1)
          _ -> pure ()
      ScenarioResolution NoResolution -> do
        iids <- allInvestigatorIds
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
          $ [ story iids noResolution
            , Record TheRitualWasCompleted
            , PlaceUnderneath (AgendaTarget agendaId) potentialSacrifices
            ]
          <> map (RemoveCampaignCard . toCardDef) sacrificedToYogSothoth
          <> removeNecronomicon
          <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 1) -> do
        iids <- allInvestigatorIds
        xp <- getXp
        removeNecronomicon <- getRemoveNecronomicon
        pushAll
          $ [ story iids resolution1
            , Record TheInvestigatorsPutSilasBishopOutOfHisMisery
            ]
          <> map (RemoveCampaignCard . toCardDef) sacrificed
          <> removeNecronomicon
          <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 2) -> do
        iids <- allInvestigatorIds
        xp <- getXp
        pushAll
          $ [story iids resolution2, Record TheInvestigatorsRestoredSilasBishop]
          <> map (RemoveCampaignCard . toCardDef) sacrificed
          <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
          <> [EndOfGame Nothing]
        pure s
      ScenarioResolution (Resolution 3) -> do
        iids <- allInvestigatorIds
        xp <- getXp
        removeNecronomicon <- getRemoveNecronomicon
        pushAll
          $ [story iids resolution3, Record TheInvestigatorsBanishedSilasBishop]
          <> map (RemoveCampaignCard . toCardDef) sacrificed
          <> [RecordSet SacrificedToYogSothoth $ map toCardCode sacrificed]
          <> removeNecronomicon
          <> [ GainXP iid (n + 2) | (iid, n) <- xp ]
          <> [EndOfGame Nothing]
        pure s
      _ -> BloodOnTheAltar . (`with` metadata) <$> runMessage msg attrs
