module Arkham.Scenario.Scenarios.IntoTheMaelstrom (IntoTheMaelstrom (..), intoTheMaelstrom) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator (getMaybeLocation, withLocationOf)
import Arkham.Helpers.Query
import Arkham.Helpers.Xp (toBonus)
import Arkham.I18n
import Arkham.Key
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Grid
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.IntoTheMaelstrom.Helpers
import Arkham.Trait (Trait (Yhanthlei))

newtype IntoTheMaelstrom = IntoTheMaelstrom ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheMaelstrom :: Difficulty -> IntoTheMaelstrom
intoTheMaelstrom difficulty =
  scenario
    IntoTheMaelstrom
    "07311"
    "Into the Maelstrom"
    difficulty
    []

instance HasChaosTokenValue IntoTheMaelstrom where
  getChaosTokenValue iid tokenFace (IntoTheMaelstrom attrs) = case tokenFace of
    Skull -> do
      n <- selectCount $ not_ FloodedLocation <> withTrait Yhanthlei
      pure
        $ if n >= 4
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 4 5
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 5 6
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage IntoTheMaelstrom where
  runMessage msg s@(IntoTheMaelstrom attrs) = runQueueT $ scenarioI18n $ case msg of
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
    Setup -> runScenarioSetup IntoTheMaelstrom attrs do
      setUsesGrid
      gather Set.IntoTheMaelstrom
      gather Set.AgentsOfHydra
      gather Set.CreaturesOfTheDeep
      gather Set.FloodedCaverns
      gather Set.ShatteredMemories
      gather Set.Syzygy
      gather Set.AncientEvils

      setAgendaDeck [Agendas.underTheSurface, Agendas.celestialAlignment, Agendas.theFlood]
      setActDeck [Acts.backIntoTheDepths, Acts.cityOfTheDeepV1]

      possessTheKey <- getHasRecord TheInvestigatorsPossessTheKeyToYhaNthlei
      possessAMap <- getHasRecord TheInvestigatorsPossessAMapOfYhaNthlei
      guardianDispatched <- getHasRecord TheGuardianOfYhanthleiIsDispatched
      recognized <- getHasRecord TheGatewayToYhanthleiRecognizesYouAsTheRightfulKeeper

      lead <- getLead
      investigators <- allInvestigators
      when possessTheKey do
        chooseOneM lead do
          questionLabeled "Choose an investigator to take control of the blue key"
          targets investigators (`placeKey` BlueKey)

      when possessAMap do
        chooseOneM lead do
          questionLabeled "Choose an investigator to take control of the red key"
          targets investigators (`placeKey` RedKey)

      when guardianDispatched do
        chooseOneM lead do
          questionLabeled "Choose an investigator to take control of the green key"
          targets investigators (`placeKey` GreenKey)

      when recognized do
        chooseOneM lead do
          questionLabeled "Choose an investigator to take control of the yellow key"
          targets investigators (`placeKey` YellowKey)

      let
        ks =
          [BlueKey | not possessTheKey]
            <> [RedKey | not possessAMap]
            <> [GreenKey | not guardianDispatched]
            <> [YellowKey | not recognized]
      otherKs <- shuffle [PurpleKey, WhiteKey, BlackKey]

      setAsideKeys . map UnrevealedKey =<< shuffle (take 4 $ ks <> otherKs)

      gatewayToYhanthlei <- placeInGrid (Pos 0 0) Locations.gatewayToYhanthlei
      tidalTunnels <- shuffle =<< amongGathered (CardWithTitle "Tidal Tunnel")

      for_
        ( zip
            [Pos (-1) (-1), Pos (-1) 0, Pos (-1) 1, Pos 0 (-1), Pos 0 1, Pos 1 (-1), Pos 1 0, Pos 1 1]
            tidalTunnels
        )
        (uncurry placeLocationInGrid_)

      selectEach (InvestigatorWithRecord PossessesADivingSuit) \iid -> do
        divingSuit <- genCard Assets.divingSuit
        createAssetAt_ divingSuit (InPlayArea iid)

      dagonIsAwake <- getHasRecord DagonHasAwakened

      setAside
        [ Enemies.lloigor
        , Enemies.aquaticAbomination
        , if dagonIsAwake
            then Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
            else Enemies.dagonDeepInSlumberIntoTheMaelstrom
        , Enemies.hydraDeepInSlumber
        , Acts.cityOfTheDeepV2
        , Acts.cityOfTheDeepV3
        ]

      setAside =<< amongGathered #location
      startAt gatewayToYhanthlei
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _n -> do
      case token.face of
        Cultist -> placeDoomOnAgendaAndCheckAdvance 1
        Tablet -> do
          mlid <- getMaybeLocation iid
          case mlid of
            Nothing -> assignDamage iid Tablet 1
            Just lid -> do
              canIncrease <- lid <=~> CanHaveFloodLevelIncreased
              chooseOrRunOneM iid do
                when canIncrease do
                  labeled "Increase the flood level of your location" $ increaseThisFloodLevel lid
                labeled "Take 1 damage" $ assignDamage iid Tablet 1
        ElderThing -> do
          withLocationOf iid \lid -> do
            whenM (lid <=~> LocationWithAnyKeys) $ assignHorror iid ElderThing 1
        _ -> pure ()
      pure s
    ScenarioResolution resolution -> scope "resolutions" do
      case resolution of
        NoResolution -> do
          story $ i18nWithTitle "noResolution"
          push R8
        Resolution 1 -> do
          story $ i18nWithTitle "resolution1"
          record TheInvestigatorsEscapedYhanthlei
          selectEach
            ( mapOneOf
                enemyIs
                [ Enemies.dagonDeepInSlumberIntoTheMaelstrom
                , Enemies.dagonAwakenedAndEnragedIntoTheMaelstrom
                , Enemies.hydraDeepInSlumber
                , Enemies.hydraAwakenedAndEnraged
                ]
            )
            addToVictory
          shatteredTheAlignment <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Acts.cityOfTheDeepV1
          conspiracyFulfilled <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Acts.cityOfTheDeepV2
          conspiracyDismantled <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Acts.cityOfTheDeepV3
          if shatteredTheAlignment && (conspiracyFulfilled || conspiracyDismantled)
            then allGainXpWithBonus attrs $ toBonus "bonus" 3
            else allGainXp attrs
          eachInvestigator (`sufferPhysicalTrauma` 2)
          push $ if shatteredTheAlignment then R2 else R3
        Resolution 2 -> do
          story $ i18nWithTitle "resolution2"
          record ThePlotOfTheDeepOnesWasThwarted
          conspiracyFulfilled <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Acts.cityOfTheDeepV2
          if conspiracyFulfilled
            then push R4
            else do
              conspiracyDismantled <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Acts.cityOfTheDeepV3
              if conspiracyDismantled
                then push R5
                else endOfScenario
        Resolution 3 -> do
          story $ i18nWithTitle "resolution3"
          record TheFloodHasBegun
          conspiracyFulfilled <- selectAny $ VictoryDisplayCardMatch $ basic $ cardIs Acts.cityOfTheDeepV2
          if conspiracyFulfilled
            then push R6
            else push R7
        Resolution 4 -> do
          story $ i18nWithTitle "resolution4"
          record AgentHarpersMissionIsComplete
          -- eachInvestigator \iid -> gainXp iid ScenarioSource (ikey "xp.bonus") 3
          endOfScenario
        Resolution 5 -> do
          story $ i18nWithTitle "resolution5"
          record TheRichesOfTheDeepAreLostForever
          -- eachInvestigator \iid -> gainXp iid ScenarioSource (ikey "xp.bonus") 3
          endOfScenario
        Resolution 6 -> do
          story $ i18nWithTitle "resolution6"
          record AgentHarpersMissionIsCompleteButAtWhatCost
          eachInvestigator (`sufferMentalTrauma` 2)
          gameOver
        Resolution 7 -> do
          story $ i18nWithTitle "resolution7"
          record TheRichesOfTheDeepAreLostForeverButAtWhatCost
          eachInvestigator (`sufferMentalTrauma` 2)
          gameOver
        Resolution 8 -> do
          story $ i18nWithTitle "resolution8"
          record TheDeepOnesHaveFloodedTheEarth
          eachInvestigator (kill attrs)
          gameOver
        _ -> error "Unknown resolution"
      pure s
    _ -> IntoTheMaelstrom <$> liftRunMessage msg attrs
