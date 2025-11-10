module Arkham.Scenario.Scenarios.DancingMad (dancingMad) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.FlavorText
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest (getSkillTest, withSkillTest)
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Name (toTitle)
import Arkham.Placement
import Arkham.Projection
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DancingMad.Helpers
import Arkham.Trait (Trait (Coterie, Detective, Police))
import Data.Map.Strict qualified as Map

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
    Skull -> do
      let maxValue = if attrs.difficulty == Easy then 3 else 5
      x <- min maxValue <$> selectCount (EnemyWithPlacement InTheShadows)
      pure $ ChaosTokenValue Cultist (if x == 0 then ZeroModifier else NegativeModifier x)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 4 6
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 1)
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

      let otherLocations =
            [ Locations.elMalecon
            , Locations.jardinesDeLaTropical
            , Locations.granTeatroDeLaHabana
            , Locations.miramarYachtClub
            , Locations.plazaHotel
            ]

      if n == 1
        then do
          placeAll otherLocations
          setAside [Enemies.desiderioDelgadoAlvarez106]
          investigators <- allInvestigators
          desi <- fromGathered1 Assets.desiderioDelgadoAlvarez
          leadChooseOneM do
            questionLabeledCard desi
            portraits investigators $ createAssetAt_ desi . InPlayArea
        else do
          setAside
            $ [ Enemies.desiderioDelgadoAlvarez106
              , Enemies.desiderioDelgadoAlvarez107
              , Enemies.otherworldlyMimic
              , Enemies.otherworldlyMimic
              , Enemies.paradigmEfficer
              , Enemies.paradigmEfficer
              ]
            <> otherLocations
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
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist -> withLocationOf iid $ makeDecoyAt iid
        Tablet -> whenJustM getSkillTest \st -> do
          for_ (Map.assocs st.committedCards) \(iid', cards) -> do
            for_ cards \c -> when (cardMatch c NonWeakness) $ hollow iid' c
        _ -> pure ()
      pure s
    ResolveChaosToken token ElderThing iid -> do
      withLocationOf iid \loc -> do
        anyConcealed <- fieldMap LocationConcealedCards (not . null) loc
        when anyConcealed do
          let dmg = if isEasyStandard attrs then 1 else 2
          let tkn = if isEasyStandard attrs then 3 else 5
          chooseOrRunOneM iid do
            unscoped $ countVar dmg $ labeled' "takeDamage" $ assignDamage iid ElderThing dmg
            countVar tkn $ labeled' "elderThing" do
              withSkillTest \sid ->
                skillTestModifier sid Cultist token
                  $ ChangeChaosTokenModifier (NegativeModifier tkn)
      pure s
    ScenarioSpecific "removedHollow" value -> do
      let card = toResult @Card value
      case card.owner of
        Nothing -> pure s
        Just iid -> do
          let hollowed = getMetaKeyDefault "removedHollows" mempty attrs
          pure $ DancingMad $ attrs & setMetaKey "removedHollows" (Map.insertWith (<>) iid [card] hollowed)
    _ -> DancingMad <$> liftRunMessage msg attrs
