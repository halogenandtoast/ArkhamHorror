module Arkham.Scenario.Scenarios.FatalMirage (fatalMirage) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Agenda
import Arkham.Helpers.Log
import Arkham.Helpers.Text
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Placement
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Trait (Trait (Eidolon, Resolute))

newtype FatalMirage = FatalMirage ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fatalMirage :: Difficulty -> FatalMirage
fatalMirage difficulty =
  scenario
    FatalMirage
    "08549"
    "Fatal Mirage"
    difficulty
    [ ".           ottomanFront       .                desertedStation  alaskanWilds"
    , "airfield    .                  baseCamp         .                infirmary"
    , ".           coastalWaters      prisonOfMemories riverviewTheatre ."
    , "moaiStatues deckOfTheTheodosia .                universityHalls  drKenslersOffice"
    , ".           hedgeMaze          standingStones   elderChamber     clutteredDormitory"
    , ".           dyersClassroom     .                theBlackStone    ."
    ]

instance HasChaosTokenValue FatalMirage where
  getChaosTokenValue iid tokenFace (FatalMirage attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ VictoryDisplayCardMatch $ basic #story
      m <- getCurrentAgendaStep
      pure $ toChaosTokenValue attrs Skull n (n + m)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs Tablet 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage FatalMirage where
  runMessage msg s@(FatalMirage attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      killedInThePlaneCrash <- getRecordSet WasKilledInThePlaneCrash
      when (recorded Assets.professorWilliamDyerProfessorOfGeology.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "dyerWasKilledInThePlaneCrash"

      when (recorded Assets.roaldEllsworthIntrepidExplorer.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "ellsworthWasKilledInThePlaneCrash"

      when (recorded Assets.eliyahAshevakDogHandler.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "ashevakWasKilledInThePlaneCrash"

      when (recorded Assets.danforthBrilliantStudent.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "danforthWasKilledInThePlaneCrash"

      when (recorded Assets.jamesCookieFredericksDubiousChoice.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "cookieWasKilledInThePlaneCrash"

      when (recorded Assets.averyClaypoolAntarcticGuide.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "claypoolWasKilledInThePlaneCrash"

      when (recorded Assets.takadaHirokoAeroplaneMechanic.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "takadaWasKilledInThePlaneCrash"

      when (recorded Assets.drMalaSinhaDaringPhysician.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "sinhaWasKilledInThePlaneCrash"

      when (recorded Assets.drAmyKenslerProfessorOfBiology.cardCode `elem` killedInThePlaneCrash) do
        blueStory $ i18nEntry "kenslerWasKilledInThePlaneCrash"

      eachInvestigator (`forInvestigator` PreScenarioSetup)
      pure s
    ForInvestigator iid PreScenarioSetup -> do
      partners <- getRemainingPartners
      unless (null partners) do
        chooseOneM iid do
          questionLabeled "Choose a partner for this scenario"
          labeled "Do not take a partner" nothing
          for_ partners \partner -> do
            inPlay <- selectAny $ assetIs partner.cardCode
            unless inPlay do
              cardLabeled partner.cardCode $ handleTarget iid ScenarioSource (CardCodeTarget partner.cardCode)
      pure s
    HandleTargetChoice iid (isSource attrs -> True) (CardCodeTarget cardCode) -> do
      for_ (lookupCardDef cardCode) \def -> do
        card <- genCard def
        assetId <- createAssetAt card (InPlayArea iid)
        partner <- getPartner cardCode
        pushWhen (partner.damage > 0) $ Msg.PlaceDamage CampaignSource (toTarget assetId) partner.damage
        pushWhen (partner.horror > 0) $ Msg.PlaceHorror CampaignSource (toTarget assetId) partner.horror
      pure s
    Setup -> runScenarioSetup FatalMirage attrs do
      gather Set.FatalMirage
      gather Set.AgentsOfTheUnknown
      gather Set.LeftBehind
      gather Set.Miasma
      gather Set.NamelessHorrors
      gather Set.SilenceAndMystery
      gather Set.Tekelili
      gather Set.ChillingCold

      setAgendaDeck [Agendas.etherealTangleV1]
      setActDeck [Acts.shadowOfThePastV1]

      startAt =<< place Locations.prisonOfMemories
      setAside =<< amongGathered #location
      setAside
        [ Enemies.memoryOfAHuntGoneAwry
        , Enemies.memoryOfALostPatient
        , Enemies.memoryOfAMissingFather
        , Enemies.memoryOfARavagedCountry
        , Enemies.memoryOfARegretfulVoyage
        , Enemies.memoryOfAnUnspeakableEvil
        , Enemies.memoryOfATerribleDiscovery
        , Enemies.memoryOfAnAlienTranslation
        , Enemies.memoryOfAnUnrequitedLove
        ]
      setAside =<< amongGathered (CardWithTrait Resolute)

      case attrs.difficulty of
        Expert -> placeDoomOnAgenda 2
        Hard -> placeDoomOnAgenda 1
        _ -> pure ()

      addTekeliliDeck
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist | n < 2 -> placeCluesOnLocation iid Cultist 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist -> placeCluesOnLocation iid Cultist 1
        Tablet -> do
          atPrison <- iid <=~> investigatorAt (locationIs Locations.prisonOfMemories)
          if isHardExpert attrs && n >= 2
            then do
              unless atPrison $ moveTo_ Tablet iid Locations.prisonOfMemories
              addTekelili iid . take 1 =<< getScenarioDeck TekeliliDeck
            else do
              chooseOneM iid do
                unless atPrison do
                  labeled "Move to the Prison of Memories" $ moveTo_ Tablet iid Locations.prisonOfMemories
                labeled "Shuffle the top card of the Tekeli-li deck into your deck without looking at it." do
                  addTekelili iid . take 1 =<< getScenarioDeck TekeliliDeck
        ElderThing ->
          chooseSelectM iid (EnemyWithTrait Eidolon) \enemy -> placeDoom ElderThing enemy 1
        _ -> pure ()
      pure s
    _ -> FatalMirage <$> liftRunMessage msg attrs
