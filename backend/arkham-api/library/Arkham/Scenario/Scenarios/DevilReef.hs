module Arkham.Scenario.Scenarios.DevilReef (DevilReef (..), devilReef) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Campaigns.TheInnsmouthConspiracy.Memory
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Exception
import Arkham.Investigator.Types (Field(..))
import Arkham.Helpers.Agenda
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy, isFightWith, isEvadeWith)
import Arkham.Helpers.Log
import Arkham.Helpers.Query
import Arkham.I18n
import Arkham.Key
import Arkham.Trait (Trait(DeepOne))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import Arkham.Matcher hiding (assetAt)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Placement qualified as P
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.DevilReef.Helpers

newtype DevilReef = DevilReef ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilReef :: Difficulty -> DevilReef
devilReef difficulty = scenario DevilReef "07163" "Devil Reef" difficulty []

instance HasChaosTokenValue DevilReef where
  getChaosTokenValue iid tokenFace (DevilReef attrs) = case tokenFace of
    Skull -> do
      ks <- selectAgg id InvestigatorKeys UneliminatedInvestigator
      pure $ toChaosTokenValue attrs Skull (length ks) (length ks + 1)
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 4 5
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage DevilReef where
  runMessage msg s@(DevilReef attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      story $ i18nWithTitle "intro1"
      missionWasSuccessful <- getHasRecord TheMissionWasSuccessful
      doStep (if missionWasSuccessful then 2 else 3) msg
      selectForMaybeM (InDeckOf Anyone <> basic (cardIs Assets.elinaHarperKnowsTooMuch)) obtainCard
      whenHasRecord TheMissionFailed do
        lead <- getLead
        investigators <- allInvestigators
        thomasDawson <- genCard Assets.thomasDawsonSoldierInANewWar
        chooseOneM lead do
          questionLabeled "Choose an investigator to shuffle Thomas Dawson into"
          targets investigators \iid -> shuffleCardsIntoDeck iid (only thomasDawson)

      pure s
    DoStep 2 PreScenarioSetup -> do
      story $ i18nWithTitle "intro2"
      pure s
    DoStep 3 PreScenarioSetup -> do
      story $ i18nWithTitle "intro3"
      pure s
    StandaloneSetup -> do
      {- FOURMOLU_DISABLE -}
      setChaosTokens
        [ #"+1", #"0", #"0", #"-1", #"-1", #"-1", #"-2", #"-2", #"-3", #"-4"
        , Skull, Skull, Cultist, Cultist, Tablet, Tablet, ElderThing, ElderThing
        , AutoFail, ElderSign
        ]
      {- FOURMOLU_ENABLE -}
      pure s
    Setup -> runScenarioSetup DevilReef attrs do
      gather Set.DevilReef
      gather Set.AgentsOfHydra
      gather Set.CreaturesOfTheDeep
      gather Set.FloodedCaverns
      gather Set.Malfunction
      gather Set.RisingTide

      whenHasRecord TheMissionWasSuccessful do
        lead <- getLead
        investigators <- allInvestigators
        thomasDawson <- genCard Assets.thomasDawsonSoldierInANewWar
        chooseOneM lead do
          questionLabeled "Choose an investigator to add Thomas Dawson into their hand"
          targets investigators \iid -> addToHand iid (only thomasDawson)

      aBattle <- hasMemory ABattleWithAHorrifyingDevil

      let agenda1 = if aBattle then Agendas.secretsOfTheSeaV1 else Agendas.secretsOfTheSeaV2

      setAgendaDeck [agenda1, Agendas.theDevilOfTheDepths]
      setActDeck [Acts.reefOfMysteries]

      setAsideKeys [PurpleKey, WhiteKey, BlackKey]
      setAsideKeys . map UnrevealedKey =<< shuffleM [YellowKey, GreenKey, RedKey, BlueKey]

      churningWaters <- placeInGrid (Pos 0 0) Locations.churningWaters
      push $ SetFloodLevel churningWaters FullyFlooded
      fishingVessel <- assetAt Assets.fishingVessel churningWaters
      eachInvestigator $ \iid -> push $ PlaceInvestigator iid (InVehicle fishingVessel)
      reveal churningWaters

      setAside [Assets.wavewornIdol, Assets.awakenedMantle, Assets.headdressOfYhaNthlei]

      cyclopeanRuins <- pickFrom (Locations.cyclopeanRuins_176a, Locations.cyclopeanRuins_176b)
      deepOneGrotto <- pickFrom (Locations.deepOneGrotto_175a, Locations.deepOneGrotto_175b)
      templeOfTheUnion <- pickFrom (Locations.templeOfTheUnion_177a, Locations.templeOfTheUnion_177b)

      setAside [cyclopeanRuins, deepOneGrotto, templeOfTheUnion]

      zipWithM_ placeInGrid [Pos 0 3, Pos 4 2, Pos (-4) 2, Pos 4 (-2), Pos (-4) (-2)]
        =<< shuffleM
          [ Locations.lonelyIsle
          , Locations.hiddenCove
          , Locations.wavewornIsland
          , Locations.saltMarshes
          , Locations.blackReef
          ]
      addExtraDeck TidalTunnelDeck =<< shuffle =<< amongGathered (CardWithTitle "Tidal Tunnel")
    PlaceKey (InvestigatorTarget iid) PurpleKey -> do
      selectForMaybeM (assetIs Assets.wavewornIdol) $ takeControlOfAsset iid
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey (InvestigatorTarget iid) WhiteKey -> do
      selectForMaybeM (assetIs Assets.awakenedMantle) $ takeControlOfAsset iid
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey (InvestigatorTarget iid) BlackKey -> do
      selectForMaybeM (assetIs Assets.headdressOfYhaNthlei) $ takeControlOfAsset iid
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey (ActTarget _) PurpleKey -> do
      selectForMaybeM (assetIs Assets.wavewornIdol) removeFromGame
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey (ActTarget _) WhiteKey -> do
      selectForMaybeM (assetIs Assets.awakenedMantle) removeFromGame
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey (ActTarget _) BlackKey -> do
      selectForMaybeM (assetIs Assets.headdressOfYhaNthlei) removeFromGame
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey target PurpleKey -> do
      selectForMaybeM (assetIs Assets.wavewornIdol) (`P.place` Near target)
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey target WhiteKey -> do
      selectForMaybeM (assetIs Assets.awakenedMantle) (`P.place` Near target)
      DevilReef <$> liftRunMessage msg attrs
    PlaceKey target BlackKey -> do
      selectForMaybeM (assetIs Assets.headdressOfYhaNthlei) (`P.place` Near target)
      DevilReef <$> liftRunMessage msg attrs
    ResolveChaosToken _ Cultist iid -> do
      when (isHardExpert attrs) do
        whenM (orM [isEvadeWith (withTrait DeepOne), isFightWith (withTrait DeepOne)]) do
          getSkillTestTargetedEnemy >>= traverse_ \eid -> do
            engagedWithYou <- eid <=~> enemyEngagedWith iid
            if engagedWithYou
              then pushAll [DisengageEnemy iid eid, EnemyEngageInvestigator eid iid]
              else push $ EnemyEngageInvestigator eid iid
      pure s
    ResolveChaosToken _ Tablet iid -> do
      when (isHardExpert attrs) do
        whenM (iid <!=~> InVehicleMatching AnyAsset) $ assignDamage iid Tablet 1
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      when (isHardExpert attrs) do
        whenM (selectAny $ locationWithInvestigator iid <> LocationWithAnyKeys) do
          assignHorror iid ElderThing 1
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (isEasyStandard attrs) do
        case token.face of
          Cultist -> whenM (orM [isEvadeWith (withTrait DeepOne), isFightWith (withTrait DeepOne)]) do
            getSkillTestTargetedEnemy >>= traverse_ \eid -> do
              engagedWithYou <- eid <=~> enemyEngagedWith iid
              if engagedWithYou
                then pushAll [DisengageEnemy iid eid, EnemyEngageInvestigator eid iid]
                else push $ EnemyEngageInvestigator eid iid
          Tablet -> whenM (iid <!=~> InVehicleMatching AnyAsset) $ assignDamage iid Tablet 1
          ElderThing -> whenM (selectAny $ locationWithInvestigator iid <> LocationWithAnyKeys) do
            assignHorror iid ElderThing 1
          _ -> pure ()
      pure s
    ScenarioResolution resolution -> do
      case resolution of
        NoResolution -> push R1
        Resolution 1 -> do
          story $ scope "resolutions" $ i18nWithTitle "resolution1"

          n <- getCurrentAgendaStep
          terrorOfTheDevilReef <- selectOne $ EnemyWithTitle "The Terror of Devil Reef"

          when (n == 1 || isJust terrorOfTheDevilReef) do
            record TheTerrorOfDevilReefIsStillAlive

          whenAny (VictoryDisplayCardMatch $ basic $ CardWithTitle "The Terror of Devil Reef") do
            record TheTerrorOfDevilReefIsDead

          allGainXp attrs

          actKeys <- field ActKeys =<< selectJust AnyAct
          purple <- ((PurpleKey `elem` actKeys) ||) <$> selectAny (InvestigatorWithKey PurpleKey)
          white <- ((WhiteKey `elem` actKeys) ||) <$> selectAny (InvestigatorWithKey WhiteKey)
          black <- ((BlackKey `elem` actKeys) ||) <$> selectAny (InvestigatorWithKey BlackKey)

          let
            interludeKey = case (purple, white, black) of
              (True, True, True) -> HasPurpleWhiteAndBlackKeys
              (True, True, False) -> HasPurpleAndWhiteKeys
              (True, False, True) -> HasPurpleAndBlackKeys
              (False, True, True) -> HasWhiteAndBlackKeys
              (True, False, False) -> HasPurpleKey
              (False, True, False) -> HasWhiteKey
              (False, False, True) -> HasBlackKey
              _ -> HasNoKeys
          endOfScenarioThen (InterludeStep 3 (Just interludeKey))
        _ -> throw $ UnknownResolution resolution
      pure s
    _ -> DevilReef <$> liftRunMessage msg attrs
