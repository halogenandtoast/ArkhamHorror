module Arkham.Scenario.Scenarios.ThePitOfDespair (ThePitOfDespair (..), thePitOfDespair) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.ChaosToken
import Arkham.Difficulty
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Investigator
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Investigator.Projection ()
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ThePitOfDespair.Helpers
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype ThePitOfDespair = ThePitOfDespair ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePitOfDespair :: Difficulty -> ThePitOfDespair
thePitOfDespair difficulty = scenario ThePitOfDespair "07041" "The Pit of Despair" difficulty []

instance HasChaosTokenValue ThePitOfDespair where
  getChaosTokenValue iid tokenFace (ThePitOfDespair attrs) = case tokenFace of
    Skull -> do
      floodLevel <-
        maybe (pure Unflooded) (fieldWithDefault Unflooded LocationFloodLevel) =<< getMaybeLocation iid
      pure $ case floodLevel of
        FullyFlooded -> toChaosTokenValue attrs Skull 3 4
        PartiallyFlooded -> toChaosTokenValue attrs Skull 2 3
        _ -> toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 3)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ThePitOfDespair where
  runMessage msg s@(ThePitOfDespair attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-4"
        , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , ElderThing
        , AutoFail , ElderSign
        ]
      {- FOURMOLU_ENABLE -}
      pure s
    Setup -> runScenarioSetup ThePitOfDespair attrs do
      gather Set.ThePitOfDespair
      gather Set.CreaturesOfTheDeep
      gather Set.FloodedCaverns
      gather Set.RisingTide
      gather Set.ShatteredMemories
      gather Set.AgentsOfCthulhu
      gather Set.Rats

      setAgendaDeck [Agendas.awakening, Agendas.theWaterRises, Agendas.sacrificeForTheDeep]
      setActDeck [Acts.thePit, Acts.theEscape]

      startAt =<< placeInGrid (Pos 0 0) Locations.unfamiliarChamber
      setAside [Locations.idolChamber, Locations.altarToDagon, Locations.sealedExit]

      randomizedKeys <- shuffleM $ map UnrevealedKey [RedKey, YellowKey, PurpleKey]
      setAsideKeys $ BlueKey : GreenKey : randomizedKeys

      (inPlayTidalTunnels, tidalTunnelDeck) <-
        splitAt 3
          <$> shuffleM
            [ Locations.boneRiddenPit
            , Locations.fishGraveyard
            , Locations.underwaterCavern
            , Locations.underwaterCavern
            , Locations.tidalPool
            , Locations.tidalPool
            , Locations.undergroundRiver
            , Locations.undergroundRiver
            ]
      addExtraDeck TidalTunnelDeck tidalTunnelDeck
      for_ (zip [Pos (-1) 0, Pos 1 0, Pos 0 (-1)] inPlayTidalTunnels) (uncurry placeInGrid)

      setAside
        [ Enemies.theAmalgam
        , Treacheries.blindsense
        , Treacheries.blindsense
        , Treacheries.fromTheDepths
        , Treacheries.fromTheDepths
        , Treacheries.fromTheDepths
        ]
    ResolveChaosToken _ Cultist iid -> do
      when (isHardExpert attrs) do
        whenAny (locationWithInvestigator iid <> FloodedLocation) $ assignDamage iid Cultist 1
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        whenM (notNull <$> iid.keys) $ assignHorror iid Tablet 1
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      when (isHardExpert attrs) do
        selectOne (OutOfPlayEnemy TheDepths $ enemyIs Enemies.theAmalgam) >>= traverse_ \eid -> do
          withLocationOf iid \lid -> push $ EnemySpawnFromOutOfPlay TheDepths (Just iid) lid eid
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (isEasyStandard attrs) do
        case token.face of
          Cultist -> whenAny (locationWithInvestigator iid <> FloodedLocation) do
            assignDamage iid Cultist 1
          Tablet -> whenM (notNull <$> iid.keys) $ assignHorror iid Tablet 1
          ElderThing ->
            selectOne (OutOfPlayEnemy TheDepths $ enemyIs Enemies.theAmalgam) >>= traverse_ \eid -> do
              withLocationOf iid \lid -> push $ EnemySpawnFromOutOfPlay TheDepths (Just iid) lid eid
          _ -> pure ()
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> story $ i18n "noResolution"
        Resolution 1 -> story $ i18n "resolution1"
        other -> throwIO $ UnknownResolution other
      allGainXp attrs
      endOfScenario
      pure s
    _ -> ThePitOfDespair <$> liftRunMessage msg attrs
