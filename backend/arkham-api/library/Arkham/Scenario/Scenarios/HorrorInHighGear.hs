module Arkham.Scenario.Scenarios.HorrorInHighGear (HorrorInHighGear (..), horrorInHighGear) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Direction
import Arkham.EncounterSet qualified as Set
import Arkham.Exception
import Arkham.Helpers.Query (getLead, getPlayerCount)
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (assetAt)
import Arkham.Message.Lifted.Choose
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.HorrorInHighGear.Helpers
import Arkham.Trait (Trait (Vehicle))

newtype HorrorInHighGear = HorrorInHighGear ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

horrorInHighGear :: Difficulty -> HorrorInHighGear
horrorInHighGear difficulty =
  scenario
    HorrorInHighGear
    "07198"
    "Horror in High Gear"
    difficulty
    [ ".      .      .    "
    , ".      .      .    "
    , "road1a road2a road3a"
    , "road1a road2a road3a"
    , ".      .      .    "
    , ".      .      .    "
    ]

instance HasChaosTokenValue HorrorInHighGear where
  getChaosTokenValue iid tokenFace (HorrorInHighGear attrs) = case tokenFace of
    Skull -> do
      useHigher <- (<= 6) . length <$> getRoadDeck
      pure $ toChaosTokenValue attrs Skull (if useHigher then 3 else 1) (if useHigher then 4 else 2)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 2
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 4)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage HorrorInHighGear where
  runMessage msg s@(HorrorInHighGear attrs) = runQueueT $ scenarioI18n $ case msg of
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-4"
        , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , ElderThing
        , AutoFail , ElderSign
        ]
      {- FOURMOLU_ENABLE -}
      pure s
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    Setup -> runScenarioSetup HorrorInHighGear attrs do
      gather Set.HorrorInHighGear
      gather Set.FogOverInnsmouth
      gather Set.Malfunction
      gather Set.ShatteredMemories
      gather Set.AncientEvils

      theTerrorOfDevilReefIsDead <- getHasRecord TheTerrorOfDevilReefIsDead
      let agenda1 = if theTerrorOfDevilReefIsDead then Agendas.theChaseIsOnV2 else Agendas.theChaseIsOnV1

      setAgendaDeck [agenda1, Agendas.hotPursuit]
      setActDeck [Acts.pedalToTheMetal]

      (bottom, top) <-
        splitAt 2
          <$> shuffleM
            [ Locations.dimlyLitRoad_a
            , Locations.dimlyLitRoad_b
            , Locations.dimlyLitRoad_c
            , Locations.cliffsideRoad_a
            , Locations.cliffsideRoad_b
            , Locations.forkInTheRoad_a
            , Locations.forkInTheRoad_b
            , Locations.intersection_a
            , Locations.intersection_b
            , Locations.tightTurn_a
            , Locations.tightTurn_b
            , Locations.tightTurn_c
            , Locations.desolateRoad_a
            , Locations.desolateRoad_b
            ]

      bottom' <- shuffleM $ Locations.falconPointApproach : bottom
      setAside $ replicate 6 Locations.longWayAround

      let (inPlay, roadDeck) = splitAt 3 (top <> bottom')

      placed <- for (withIndex1 inPlay) $ \(n, location) -> placeLabeled ("road" <> tshow n <> "a") location

      for_ (zip placed (drop 1 placed)) \(left, right) -> do
        push $ PlacedLocationDirection left LeftOf right

      for_ (headMay $ reverse placed) \front -> do
        assetAt_ Assets.thomasDawsonsCarRunning front
        assetAt_ Assets.elinaHarpersCarRunning front
        eachInvestigator (`forInvestigator` DoStep 1 Setup)
        doStep 2 Setup
        reveal front

      addExtraDeck RoadDeck roadDeck

      lead <- getLead
      getPlayerCount >>= \case
        2 -> findEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
        3 -> findEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
        4 -> do
          findEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
          findEncounterCard lead ScenarioTarget (#enemy <> CardWithTrait Vehicle)
        _ -> pure ()
    ForInvestigator iid (DoStep 1 Setup) -> do
      vehicles <- selectWithFilterM (AssetWithTrait Vehicle) \vehicle -> do
        passengers <- selectCount $ InVehicleMatching $ AssetWithId vehicle
        pure $ passengers < 2

      chooseOrRunOneM iid do
        questionLabeled "Which vehicle will you start in?"
        targets vehicles $ push . PlaceInvestigator iid . InVehicle
      pure s
    DoStep 2 Setup -> do
      selectEach (AssetWithTrait Vehicle) \vehicle -> do
        passengers <- select $ InVehicleMatching $ AssetWithId vehicle
        if null passengers
          then removeFromGame vehicle
          else do
            name <- field AssetName vehicle
            lead <- getLead
            chooseOrRunOneM lead do
              questionLabeled $ "Who will drive " <> toTitle name <> "?"
              targets passengers $ push . SetDriver vehicle

      pure s
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      focusCards [card] \unfocus -> do
        locations <- getRear
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "Where will the enemy spawn?"
          targets locations $ createEnemyAt_ card
        push unfocus
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ n -> do
      case token.face of
        Cultist | n > 0 -> do
          field InvestigatorPlacement iid >>= \case
            InVehicle aid -> do
              loc <- fieldJust AssetLocation aid
              passengers <-
                selectWithField InvestigatorClues $ InVehicleMatching (AssetWithId aid) <> InvestigatorWithAnyClues
              case passengers of
                [] -> pure ()
                [(x, c)] -> placeClues x loc (min c n)
                xs ->
                  if sum (map snd xs) <= n
                    then for_ xs \(x, c) -> placeClues x loc c
                    else chooseNM iid n do
                      for_ xs \(x, _c) -> clueLabeled x $ placeClues x loc 1
            _ -> pure ()
        Tablet | n > 0 -> do
          field InvestigatorPlacement iid >>= \case
            InVehicle aid -> do
              passengers <-
                selectWithField InvestigatorResources
                  $ InVehicleMatching (AssetWithId aid)
                  <> InvestigatorWithAnyResources
              case passengers of
                [] -> pure ()
                [(x, r)] -> loseResources x Tablet (min n r)
                xs ->
                  if sum (map snd xs) <= n
                    then for_ xs \(x, c) -> loseResources x Tablet c
                    else chooseNM iid n do
                      for_ xs \(x, _c) -> resourceLabeled x $ loseResources x Tablet 1
            _ -> pure ()
        ElderThing | isEasyStandard attrs -> push HuntersMove
        _ -> pure ()
      pure s
    ResolveChaosToken _ ElderThing _iid -> do
      when (isHardExpert attrs) $ push HuntersMove
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          record TheInvestigatorsReachedFalconPointAfterSunrise
        Resolution 1 -> do
          story $ i18nWithTitle "resolution1"
          record TheInvestigatorsReachedFalconPointBeforeSunrise
        _ -> throw $ UnknownResolution resolution

      allGainXp attrs
      endOfScenario
      pure s
    _ -> HorrorInHighGear <$> liftRunMessage msg attrs
