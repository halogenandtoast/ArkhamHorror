module Arkham.Scenario.Scenarios.ALightInTheFog (ALightInTheFog (..), aLightInTheFog) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field(ActKeys))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Sequence
import Arkham.Agenda.Types (Field(AgendaSequence))
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.Investigator (withLocationOf, getMaybeLocation)
import Arkham.Helpers.Log
import Arkham.Helpers.Modifiers (ModifierType(..), setActiveDuringSetup, modifySelectMaybe, modifySelectMaybeWith)
import Arkham.Helpers.Query (allInvestigators)
import Arkham.I18n
import Arkham.Id
import Arkham.Key
import Arkham.Keyword (Keyword(Aloof))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Location.Types (Field(..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenario.Types (metaL)
import Arkham.Scenarios.ALightInTheFog.Helpers
import Arkham.SortedPair
import Arkham.Story.Cards qualified as Stories
import Arkham.Treachery.Cards qualified as Treacheries

data Meta = Meta
  { captured :: [InvestigatorId]
  , resignedInMoonRoom :: [InvestigatorId]
  , relicsAddedToHand :: [CardDef]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyMeta :: Meta
emptyMeta = Meta [] [] []

capturedL :: Lens' Meta [InvestigatorId]
capturedL = lens Arkham.Scenario.Scenarios.ALightInTheFog.captured \m x -> m { captured = x }

resignedInMoonRoomL :: Lens' Meta [InvestigatorId]
resignedInMoonRoomL = lens resignedInMoonRoom \m x -> m { resignedInMoonRoom = x }

relicsAddedToHandL :: Lens' Meta [CardDef]
relicsAddedToHandL = lens relicsAddedToHand \m x -> m { relicsAddedToHand = x }

newtype ALightInTheFog = ALightInTheFog ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasModifiersFor ALightInTheFog where
  getModifiersFor (ALightInTheFog a) = do
    investigators <- modifySelectMaybe a Anyone \iid -> do
      Meta meta _ _ <- hoistMaybe $ maybeResult a.meta
      guard $ iid `elem` meta
      pure [CannotMove, CannotFight AnyEnemy, CannotBeEngaged, ScenarioModifier "captured"]
    locations <- modifySelectMaybeWith a Anywhere setActiveDuringSetup \lid -> do
      pos <- MaybeT $ field LocationPosition lid
      connected <- lift $ select $ LocationInRow pos.row
      pure [DoNotDrawConnection $ sortedPair lid connectedId | connectedId <- connected]
    pure $ investigators <> locations

aLightInTheFog :: Difficulty -> ALightInTheFog
aLightInTheFog difficulty = scenario ALightInTheFog "07231" "A Light in the Fog" difficulty []

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
    ResolveChaosToken _ Skull iid -> do
      whenAny (locationWithInvestigator iid <> FloodedLocation) do
        drawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing iid -> do
      when (isHardExpert attrs) do
        withLocationOf iid \lid -> do
          nearest <- select (NearestEnemyTo iid UnengagedEnemy)
          chooseTargetM iid nearest \enemy -> do
            temporaryModifier enemy ElderThing (RemoveKeyword Aloof) do
              moveTowardsMatching ElderThing enemy (LocationWithId lid)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _n -> do
      case token.face of
        Cultist -> afterSkillTest $ doStep 1 msg
        Tablet -> whenAny (locationWithInvestigator iid <> FloodedLocation) do
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
            else when (isHardExpert attrs) $ assignHorror iid Cultist 1
        _ -> pure ()
      pure s
    ForInvestigator iid (ScenarioSpecific "captured" _) -> do
      let meta = toResultDefault emptyMeta attrs.meta
      pure $ ALightInTheFog $ attrs & metaL .~ toJSON (meta & capturedL %~ (nub . (iid :)))
    ForInvestigator iid (ScenarioSpecific "free" _) -> do
      let meta = toResultDefault emptyMeta attrs.meta
      pure $ ALightInTheFog $ attrs & metaL .~ toJSON (meta & capturedL %~ deleteFirst iid)
    ScenarioResolution resolution -> scope "resolutions" do
      let meta = toResultDefault emptyMeta attrs.meta
      let
        defaultResolution = do
          actKeys <- field ActKeys =<< selectJust AnyAct
          recordWhen (BlackKey `elem` actKeys) TheInvestigatorsPossessAMapOfYhaNthlei
          recordWhen (RedKey `elem` actKeys) TheInvestigatorsPossessTheKeyToYhaNthlei

          investigators <- allInvestigators
          for_ (meta ^. relicsAddedToHandL) (addCampaignCardToDeckChoice investigators)
          allGainXp attrs
          endOfScenario
      case resolution of
        NoResolution -> do
          step <- agendaStep <$> selectJustField AgendaSequence AnyAgenda
          push $ if step < AgendaStep 4 then R4 else R3
        Resolution 1 -> do
          story $ i18nWithTitle "resolution1"
          for_ (meta ^. resignedInMoonRoomL) \iid -> push $ RecordForInvestigator iid PossessesADivingSuit
          defaultResolution
        Resolution 2 -> do
          story $ i18nWithTitle "resolution2"
          defaultResolution
        Resolution 3 -> do
          story $ i18nWithTitle "resolution3"
          eachInvestigator (kill attrs)
          gameOver
        Resolution 4 -> do
          story $ i18nWithTitle "resolution4"
          defaultResolution
        _ -> throw $ UnknownResolution resolution
      pure s
    Resign iid -> do
      getMaybeLocation iid >>= \case
        Nothing -> pure s
        Just lid -> do
          isMoonRoom <- lid <=~> locationIs Locations.theMoonRoom
          if isMoonRoom
            then do
              let meta = toResultDefault emptyMeta attrs.meta
              pure $ ALightInTheFog $ attrs & metaL .~ toJSON (meta & resignedInMoonRoomL %~ (nub . (iid :)))
            else pure s
    AddToHand _ cards -> do
      let relics = map toCardDef $ filterCards (mapOneOf cardIs [Assets.headdressOfYhaNthlei, Assets.awakenedMantle, Assets.wavewornIdol]) cards
      if null relics
        then pure s
        else  do
          let meta = toResultDefault emptyMeta attrs.meta
          pure $ ALightInTheFog $ attrs & metaL .~ toJSON (meta & relicsAddedToHandL %~ (nub . (relics <>)))
    _ -> ALightInTheFog <$> liftRunMessage msg attrs
