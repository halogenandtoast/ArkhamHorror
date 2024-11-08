module Arkham.Scenario.Scenarios.AtDeathsDoorstep (AtDeathsDoorstep (..), atDeathsDoorstep) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheCircleUndone.CampaignSteps (pattern TheSecretName)
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (Field (ScenarioVictoryDisplay))
import Arkham.Scenarios.AtDeathsDoorstep.Story
import Arkham.Token
import Arkham.Trait (Trait (SilverTwilight, Spectral))

newtype AtDeathsDoorstep = AtDeathsDoorstep ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atDeathsDoorstep :: Difficulty -> AtDeathsDoorstep
atDeathsDoorstep difficulty =
  scenario
    AtDeathsDoorstep
    "05065"
    "At Death's Doorstep"
    difficulty
    [ ".             .          office         .             ."
    , "billiardsRoom trophyRoom victorianHalls masterBedroom balcony"
    , ".             .          entryHall      .             ."
    ]

instance HasChaosTokenValue AtDeathsDoorstep where
  getChaosTokenValue iid chaosTokenFace (AtDeathsDoorstep attrs) = case chaosTokenFace of
    Skull -> do
      isHaunted <- selectAny $ locationWithInvestigator iid <> HauntedLocation
      pure
        . uncurry (toChaosTokenValue attrs Skull)
        $ if isHaunted then (3, 4) else (1, 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 4
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

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecordedSets =
        mapFromList
          [
            ( MissingPersons
            , [ crossedOut (toCardCode i)
              | i <-
                  [ Investigators.gavriellaMizrah
                  , Investigators.jeromeDavids
                  , Investigators.valentinoRivas
                  , Investigators.pennyWhite
                  ]
              ]
            )
          ]
    }

instance RunMessage AtDeathsDoorstep where
  runMessage msg s@(AtDeathsDoorstep attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story introPart1
      story introPart2
      story introPart3
      story introPart4
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure $ overAttrs (standaloneCampaignLogL .~ standaloneCampaignLog) s
    Setup -> runScenarioSetup AtDeathsDoorstep attrs do
      gather Set.AtDeathsDoorstep
      gather Set.SilverTwilightLodge
      gather Set.SpectralPredators
      gather Set.TrappedSpirits
      gather Set.InexorableFate
      gather Set.ChillingCold

      gatherAndSetAside Set.RealmOfDeath
      gatherAndSetAside Set.TheWatcher

      setAside
        [ Enemies.josefMeiger
        , Locations.entryHallSpectral
        , Locations.victorianHallsSpectral
        , Locations.trophyRoomSpectral
        , Locations.billiardsRoomSpectral
        , Locations.masterBedroomSpectral
        , Locations.balconySpectral
        , Locations.officeSpectral
        ]

      setAgendaDeck [Agendas.justiceXI, Agendas.overTheThreshold]
      setActDeck [Acts.hiddenAgendas, Acts.theSpectralRealm, Acts.escapeTheCage]

      entryHall <- place Locations.entryHallAtDeathsDoorstep
      startAt entryHall
      office <- place Locations.office
      billiardsRoom <- place Locations.billiardsRoom
      balcony <- place Locations.balconyAtDeathsDoorstep
      placeAll
        [ Locations.victorianHalls
        , Locations.trophyRoom
        , Locations.masterBedroom
        ]

      missingPersons <- getRecordedCardCodes MissingPersons
      evidenceLeftBehind <- getRecordCount PiecesOfEvidenceWereLeftBehind

      let gavriellaMissing = Investigators.gavriellaMizrah.cardCode `elem` missingPersons
      let jeromeMissing = Investigators.jeromeDavids.cardCode `elem` missingPersons
      let valentinoMissing = Investigators.valentinoRivas.cardCode `elem` missingPersons
      let pennyMissing = Investigators.pennyWhite.cardCode `elem` missingPersons

      -- We want to distribute the removal of clues evenly. The logic here
      -- tries to batch a groups corresponding to the number of locations we
      -- placed clues on
      when gavriellaMissing $ placeTokens attrs entryHall Clue 6
      when jeromeMissing $ placeTokens attrs office Clue 6
      when valentinoMissing $ placeTokens attrs billiardsRoom Clue 6
      when pennyMissing $ placeTokens attrs balcony Clue 6

      let
        locations =
          [entryHall | gavriellaMissing]
            <> [office | jeromeMissing]
            <> [billiardsRoom | valentinoMissing]
            <> [balcony | pennyMissing]
        doSplit 0 = []
        doSplit n =
          if n >= length locations
            then length locations : doSplit (n - length locations)
            else [n]
      when (notNull locations) do
        lead <- getLead
        for_ (doSplit evidenceLeftBehind) \n -> chooseOrRunNM lead n do
          targets locations \l -> removeTokens attrs l Clue 1
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Tablet | isEasyStandard attrs -> do
          mAction <- getSkillTestAction
          for_ mAction $ \action ->
            when (action `elem` [#fight, #evade]) $ runHauntedAbilities iid
        _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      mAction <- getSkillTestAction
      for_ mAction \action ->
        when (action `elem` [#fight, #evade]) $ runHauntedAbilities iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      isSpectralEnemy <- selectAny $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Spectral
      when isSpectralEnemy do
        assignDamageAndHorror iid ElderThing 1 (if isHardExpert attrs then 1 else 0)
      pure s
    ScenarioResolution NoResolution -> do
      getCurrentActStep >>= \case
        1 -> push R2
        2 -> push R3
        3 -> push R1
        _ -> error "Invalid act step"
      pure s
    ScenarioResolution (Resolution n) -> do
      entryHall <- selectJust $ LocationWithTitle "Entry Hall"
      inVictory <- isInVictoryDisplay Enemies.josefMeiger
      underEntryHall <-
        fieldMap
          LocationCardsUnderneath
          ((elem Enemies.josefMeiger) . map toCardDef)
          entryHall
      silverTwilightInVictory <-
        scenarioFieldMap
          ScenarioVictoryDisplay
          (count (`cardMatch` CardWithTrait SilverTwilight))
      silverTwilightUnderEntryHall <-
        fieldMap
          LocationCardsUnderneath
          (count (`cardMatch` CardWithTrait SilverTwilight))
          entryHall

      let
        interludeKey
          | inVictory = ThePriceOfProgress4
          | underEntryHall = ThePriceOfProgress5
          | silverTwilightInVictory >= silverTwilightUnderEntryHall = ThePriceOfProgress4
          | otherwise = ThePriceOfProgress6

      case n of
        1 -> do
          story resolution1
          record TheInvestigatorsEscapedTheSpectralRealm
          allGainXp attrs
          endOfScenarioThen $ InterludeStep 2 (Just interludeKey)
        2 -> do
          story resolution2
          record TheInvestigatorsLearnedNothingOfTheLodge'sSchemes
          allGainXp attrs
          endOfScenarioThen $ UpgradeDeckStep TheSecretName
        3 -> do
          story resolution3
          record TheInvestigatorsAreNeverSeenOrHeardFromAgain
          allGainXp attrs
          endOfScenarioThen $ UpgradeDeckStep TheSecretName
        _ -> error "Invalid resolution"

      pure s
    _ -> AtDeathsDoorstep <$> liftRunMessage msg attrs
