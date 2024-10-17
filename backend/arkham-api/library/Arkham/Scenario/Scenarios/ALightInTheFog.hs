{-# OPTIONS_GHC -Wno-deprecations #-}
module Arkham.Scenario.Scenarios.ALightInTheFog (ALightInTheFog (..), aLightInTheFog) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Log
import Arkham.Helpers.Investigator (withLocationOf)
import Arkham.Message.Lifted.Choose
import Arkham.Key
import Arkham.Keyword (Keyword(Aloof))
import Arkham.Modifier
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries

newtype ALightInTheFog = ALightInTheFog ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aLightInTheFog :: Difficulty -> ALightInTheFog
aLightInTheFog difficulty =
  scenario
    ALightInTheFog
    "07231"
    "A Light in the Fog"
    difficulty
    []

instance HasChaosTokenValue ALightInTheFog where
  getChaosTokenValue iid tokenFace (ALightInTheFog attrs) = case tokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 3)
    ElderThing -> pure $ ChaosTokenValue ElderThing (NegativeModifier 4)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage ALightInTheFog where
  runMessage msg s@(ALightInTheFog attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro"
      pure s
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1" , #"0" , #"0" , #"-1" , #"-1" , #"-2" , #"-2" , #"-3" , #"-4"
        , Skull , Skull , Cultist , Cultist , Tablet , Tablet , ElderThing , ElderThing
        , AutoFail , ElderSign
        ]
      {- FOURMOLU_ENABLE -}
      pure s
    Setup -> runScenarioSetup ALightInTheFog attrs do
      gather Set.ALightInTheFog
      gather Set.CreaturesOfTheDeep
      gather Set.FloodedCaverns
      gather Set.RisingTide
      gather Set.Syzygy
      gather Set.StrikingFear

      setAgendaDeck
        [ Agendas.fogOnTheBay
        , Agendas.unchangingAsTheSea
        , Agendas.theTideRisesALightInTheFog
        , Agendas.terrorAtFalconPoint
        ]
      setActDeck [Acts.theLighthouse, Acts.findingThePath, Acts.worshippersOfTheDeep]

      falconPointGatehouse <- placeInGrid (Pos 0 0) Locations.falconPointGatehouse
      placeInGrid_ (Pos 1 0) Locations.falconPointCliffside
      placeInGrid_ (Pos 2 0) Locations.lighthouseStairwell
      placeInGrid_ (Pos 3 0) Locations.lighthouseKeepersCottage
      placeInGrid_ (Pos 2 1) Locations.lanternRoom

      startAt falconPointGatehouse

      setAside =<< amongGathered (CardWithTitle "Tidal Tunnel" <> not_ (cardIs Locations.undergroundRiver))

      randomizedKeys <- shuffle $ map UnrevealedKey [PurpleKey, GreenKey]
      setAsideKeys $ [WhiteKey, BlackKey, BlueKey, YellowKey, RedKey] <> randomizedKeys

      placeStory Stories.captured

      setAside
        [ Enemies.oceirosMarsh
        , Treacheries.worthHisSalt
        , Treacheries.worthHisSalt
        , Treacheries.takenCaptive
        , Treacheries.takenCaptive
        , Locations.sunkenGrottoUpperDepths
        , Locations.sunkenGrottoLowerDepths
        , Locations.sunkenGrottoFinalDepths
        ]

      whenHasRecord TheIdolWasBroughtToTheLighthouse $ setAside [Assets.wavewornIdol]
      whenHasRecord TheMantleWasBroughtToTheLighthouse $ setAside [Assets.awakenedMantle]
      whenHasRecord TheHeaddressWasBroughtToTheLighthouse $ setAside [Assets.headdressOfYhaNthlei]
      whenHasRecord TheInvestigatorsReachedFalconPointAfterSunrise $ placeDoomOnAgenda 1
      whenHasRecord TheTideHasGrownStronger $ placeDoomOnAgenda 1
    ResolveChaosToken _ face iid -> do
      case face of
        Skull -> whenAny (locationWithInvestigator iid <> FloodedLocation) do
          drawAnotherChaosToken iid
        ElderThing | isHardExpert attrs -> withLocationOf iid \lid -> do
          nearest <- select (NearestEnemyTo iid UnengagedEnemy)
          chooseTargetM iid nearest \enemy -> do
            temporaryModifier enemy ElderThing (RemoveKeyword Aloof) do
              moveTowardsMatching ElderThing enemy (LocationWithId lid)
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _n -> do
      case token.face of
        Cultist -> afterSkillTest $ doStep 1 msg
        Tablet -> do
          whenAny (locationWithInvestigator iid <> FloodedLocation) do
            assignDamage iid Tablet $ if isEasyStandard attrs then 1 else 2
        ElderThing | isEasyStandard attrs -> withLocationOf iid \lid -> do
          nearest <- select (NearestEnemyTo iid UnengagedEnemy)
          chooseTargetM iid nearest \enemy -> do
            temporaryModifier enemy ElderThing (RemoveKeyword Aloof) do
              moveTowardsMatching ElderThing enemy (LocationWithId lid)

        _ -> pure ()
      pure s
    DoStep 1 (FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _n) -> do
      case token.face of
        Cultist -> withLocationOf iid \lid -> do
          canIncreaseFloodLevel <- lid <=~> CanHaveFloodLevelIncreased 
          if canIncreaseFloodLevel
            then increaseThisFloodLevel lid
            else when (isHardExpert attrs) do
              assignHorror iid Cultist 1
        _ -> pure ()
      pure s
    _ -> ALightInTheFog <$> liftRunMessage msg attrs
