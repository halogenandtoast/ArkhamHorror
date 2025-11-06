module Arkham.Scenario.Scenarios.DancingMad (dancingMad) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DancingMad.Helpers
import Arkham.Trait (Trait (Coterie, Detective, Police))

newtype DancingMad = DancingMad ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dancingMad :: Difficulty -> DancingMad
dancingMad difficulty =
  scenario
    DancingMad
    "09591"
    "Dancing Mad"
    difficulty
    [ ".     t        equals ."
    , "spade .        .      hourglass"
    , ".     squiggle square ."
    ]

instance HasChaosTokenValue DancingMad where
  getChaosTokenValue iid tokenFace (DancingMad attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ ChaosTokenValue ElderThing NoModifier
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DancingMad where
  runMessage msg s@(DancingMad attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> scope "intro" do
      policeOrDective <- selectAny $ mapOneOf InvestigatorWithTrait [Police, Detective]
      n <- getTime
      flavor do
        setTitle "title"
        p "intro1Part1"
        p.validate policeOrDective "policeOrDetective"
        p "intro1Part2"
        ul do
          li.validate (n < 20) "fewerThanTwentyTime"
          li.validate (n >= 20) "twentyOrMoreTime"
      doStep (if n < 20 then 2 else 3) PreScenarioSetup
      pure s
    DoStep 2 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro2"
      record TheCellMadeADealWithDesi
      doStep 1 Setup
      pure s
    DoStep 3 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro3"
      doStep 4 PreScenarioSetup
      pure s
    DoStep 4 PreScenarioSetup -> scope "intro" do
      flavor $ setTitle "title" >> p "intro4"
      record TheCellHasBeenAmbushed
      doStep 2 Setup
      pure s
    DoStep n Setup -> runScenarioSetup DancingMad attrs do
      gather Set.DancingMad
      gather Set.AgentsOfTheOutside
      gather Set.SecretWar
      gather Set.ShadowOfADoubt

      if n == 1
        then do
          gatherAndSetAside Set.CleanupCrew
          gatherAndSetAside Set.CrimsonConspiracy
        else do
          gather Set.CleanupCrew
          gather Set.CrimsonConspiracy

      setAgendaDeck [Agendas.silenceSpeaks, Agendas.easyPrey, Agendas.aBetrayalOfEyes]
      let act1 = if n == 1 then Acts.falseStepV1 else Acts.falseStepV2
      let act2 = if n == 1 then Acts.falseColorsV1 else Acts.falseColorsV2
      setActDeck [act1, act2, Acts.falseLight]

      startAt
        =<< place (if n == 1 then Locations.cafeLunaBastionOfRemembrance else Locations.cafeLunaCoterieHaunt)
      placeAll
        [ Locations.elMalecon
        , Locations.jardinesDeLaTropical
        , Locations.granTeatroDeLaHabana
        , Locations.miramarYachtClub
        , Locations.plazaHotel
        ]

      if n == 1
        then do
          setAside [Enemies.desiderioDelgadoAlvarez106]
          investigators <- allInvestigators
          desi <- fromGathered1 Assets.desiderioDelgadoAlvarez
          leadChooseOneM do
            questionLabeledCard desi
            portraits investigators $ createAssetAt_ desi . InPlayArea
        else do
          setAside
            [ Enemies.desiderioDelgadoAlvarez106
            , Enemies.desiderioDelgadoAlvarez107
            , Enemies.otherworldlyMimic
            , Enemies.otherworldlyMimic
            , Enemies.paradigmEfficer
            , Enemies.paradigmEfficer
            ]

          lead <- getLead
          desi <- fetchCard Enemies.desiderioDelgadoAlvarez106
          drawCard lead desi
          eachInvestigator \iid -> unless (iid == lead) $ forInvestigator iid Setup
    ForInvestigator iid Setup -> do
      enemies <- selectField EnemyName (InPlayEnemy $ EnemyWithTrait Coterie)
      discardUntilFirst
        iid
        ScenarioSource
        Deck.EncounterDeck
        (basic $ CardWithTrait Coterie <> not_ (mapOneOf (CardWithTitle . toTitle) enemies))
      shuffleEncounterDiscardBackIn
      pure s
    RequestedEncounterCard (isSource attrs -> True) (Just iid) (Just ec) -> do
      drawCard iid ec
      pure s
    _ -> DancingMad <$> liftRunMessage msg attrs
