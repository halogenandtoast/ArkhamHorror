module Arkham.Scenario.Scenarios.TheLastKing (TheLastKing (..), theLastKing) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Name hiding (labeled)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (
  crossOutRecordSetEntries,
  recordSetInsert,
  resolutionModifier,
 )
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLastKing.Story
import Arkham.Story.Cards qualified as Story
import Arkham.Token
import Arkham.Trait qualified as Trait

newtype TheLastKing = TheLastKing ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theLastKing :: Difficulty -> TheLastKing
theLastKing difficulty =
  scenario
    TheLastKing
    "03061"
    "The Last King"
    difficulty
    [ "diningRoom .         gallery"
    , "ballroom   courtyard livingRoom"
    , ".          foyer     ."
    ]

instance HasChaosTokenValue TheLastKing where
  getChaosTokenValue iid chaosTokenFace (TheLastKing attrs) = case chaosTokenFace of
    Skull -> pure $ ChaosTokenValue Skull NoModifier
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 4)
    ElderThing -> do
      shroud <- maybe (pure 0) (fieldWithDefault 0 LocationShroud) =<< field InvestigatorLocation iid
      pure $ ChaosTokenValue ElderThing (NegativeModifier shroud)
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

interviewedToCardCode :: ScenarioLogKey -> Maybe CardCode
interviewedToCardCode = \case
  InterviewedConstance -> Just $ toCardCode Assets.constanceDumaine
  InterviewedJordan -> Just $ toCardCode Assets.jordanPerry
  InterviewedHaruko -> Just $ toCardCode Assets.ishimaruHaruko
  InterviewedSebastien -> Just $ toCardCode Assets.sebastienMoreau
  InterviewedAshleigh -> Just $ toCardCode Assets.ashleighClarke
  _ -> Nothing

instance RunMessage TheLastKing where
  runMessage msg s@(TheLastKing attrs) = runQueueT $ case msg of
    PreScenarioSetup -> do
      story intro
      pure s
    StandaloneSetup -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      lead <- getLead
      addCampaignCardToDeck lead Enemies.theManInThePallidMask
      pure s
    Setup -> runScenarioSetup TheLastKing attrs do
      gather Set.TheLastKing
      gather Set.HastursGift
      gather Set.DecayAndFilth
      gather Set.TheStranger
      gather Set.AncientEvils

      startAt =<< place Locations.foyer
      otherLocations <-
        placeAllCapture
          [ Locations.courtyard
          , Locations.livingRoom
          , Locations.ballroom
          , Locations.diningRoom
          , Locations.gallery
          ]

      totalClues <- getPlayerCountValue (StaticWithPerPlayer 1 1)
      bystanders <-
        shuffleM
          =<< genCards
            [ Assets.constanceDumaine
            , Assets.jordanPerry
            , Assets.ishimaruHaruko
            , Assets.sebastienMoreau
            , Assets.ashleighClarke
            ]
      destinations <- shuffleM otherLocations
      for_ (zip bystanders (map AtLocation destinations)) \(bystander, placement) -> do
        assetId <- createAssetAt bystander placement
        placeTokens attrs assetId Clue totalClues

      setAside [Enemies.dianneDevine]

      placeUnderScenarioReference
        [ Story.sickeningReality_65
        , Story.sickeningReality_66
        , Story.sickeningReality_67
        , Story.sickeningReality_68
        , Story.sickeningReality_69
        ]
      setAgendaDeck [Agendas.fashionablyLate, Agendas.theTerrifyingTruth]
      setActDeck [Acts.discoveringTheTruth]
    ResolveChaosToken _ token iid | token `elem` [Skull, Cultist, Tablet] -> do
      case token of
        Skull -> drawAnotherChaosToken iid
        Cultist | isHardExpert attrs -> do
          clueCount <- field InvestigatorClues iid
          when (clueCount > 0) do
            push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Cultist) 1
        Tablet | isHardExpert attrs -> assignHorror iid token 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Skull -> do
          ts <-
            selectTargets
              $ if isEasyStandard attrs
                then EnemyWithTrait Trait.Lunatic
                else EnemyWithMostRemainingHealth $ EnemyWithTrait Trait.Lunatic
          when (notNull ts) $ do
            chooseOrRunOne
              iid
              [targetLabel target [PlaceTokens (ChaosTokenEffectSource Skull) target Doom 1] | target <- ts]
        Cultist | isEasyStandard attrs -> do
          clueCount <- field InvestigatorClues iid
          when
            (clueCount > 0)
            (push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Cultist) 1)
        Tablet | isEasyStandard attrs -> assignHorror iid (ChaosTokenSource token) 1
        ElderThing | isHardExpert attrs -> assignDamage iid (ChaosTokenSource token) 1
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      anyResigned <- notNull <$> select ResignedInvestigator
      push $ if anyResigned then R1 else R2
      pure s
    ScenarioResolution (Resolution n) -> do
      story $ case n of
        1 -> resolution1
        2 -> resolution2
        3 -> resolution3
        _ -> error "Invalid resolution"

      let interviewed = mapMaybe interviewedToCardCode (setToList attrs.log)
      when (notNull interviewed) $ recordSetInsert VIPsInterviewed interviewed
      when (n == 3) $ crossOutRecordSetEntries VIPsInterviewed interviewed

      vipsSlain <- selectMap toCardCode $ VictoryDisplayCardMatch $ basic $ CardWithTrait Trait.Lunatic
      when (notNull vipsSlain) $ recordSetInsert VIPsSlain vipsSlain

      -- Resolution handles XP in a special way, we must divvy up between investigators
      -- evenly and apply, this will have a weird interaction with Hospital Debts so we
      -- want to handle `getXp` in two phases. The first phase will essentially evenly
      -- add XP modifiers to the players in order to have `getXp` resolve "normally"
      clueCounts <- traverse (field ActClues) =<< select AnyAct
      investigators <- allInvestigators
      let
        extraXp = floor @Double (fromIntegral (sum clueCounts) / 2)
        (assignedXp, remainingXp) = quotRem extraXp (length investigators)
        assignXp amount iid =
          resolutionModifier attrs iid
            $ XPModifier "Clues that were on the act deck when the game ended" amount
      for_ investigators (assignXp assignedXp)

      when (remainingXp > 0) do
        lead <- getLead
        chooseNM lead remainingXp do
          for_ investigators \iid -> do
            name <- field InvestigatorName iid
            labeled ("Choose " <> display name <> " to gain 1 additional XP") $ lift $ assignXp 1 iid

      -- Assign XP now that the modifiers exist
      doStep 1 msg

      when (n == 2 || n == 3) do
        removeAllChaosTokens Cultist
        removeAllChaosTokens Tablet
        removeAllChaosTokens ElderThing
        addChaosToken Cultist
        addChaosToken Tablet
        addChaosToken ElderThing
      if n == 1 then endOfScenarioThen $ InterludeStep 1 Nothing else endOfScenario
      pure s
    DoStep 1 (ScenarioResolution _) -> do
      allGainXp attrs
      pure s
    _ -> TheLastKing <$> liftRunMessage msg attrs
