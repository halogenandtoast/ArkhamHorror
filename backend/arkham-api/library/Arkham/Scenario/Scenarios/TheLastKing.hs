module Arkham.Scenario.Scenarios.TheLastKing (setupTheLastKing, theLastKing, TheLastKing (..)) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignStep
import Arkham.Campaigns.ThePathToCarcosa.Key
import Arkham.Card
import Arkham.Classes
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Exception
import Arkham.Helpers.FlavorText
import Arkham.Helpers.GameValue
import Arkham.Helpers.Query
import Arkham.Helpers.Xp
import Arkham.I18n
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Arkham.Modifier
import Arkham.Name hiding (labeled)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Import.Lifted
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheLastKing.Helpers
import Arkham.Story.Cards qualified as Story
import Arkham.Token
import Arkham.Trait qualified as Trait
import Arkham.Treachery.Cards qualified as Treacheries

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
  InterviewedConstance -> Just Assets.constanceDumaine.cardCode
  InterviewedJordan -> Just Assets.jordanPerry.cardCode
  InterviewedHaruko -> Just Assets.ishimaruHaruko.cardCode
  InterviewedSebastien -> Just Assets.sebastienMoreau.cardCode
  InterviewedAshleigh -> Just Assets.ashleighClarke.cardCode
  _ -> Nothing

setupTheLastKing :: (HasI18n, ReverseQueue m) => ScenarioAttrs -> ScenarioBuilderT m ()
setupTheLastKing attrs = do
  setup do
    ul do
      li "gatherSets"
      li "placeLocations"
      li.nested "bystanders.instructions" do
        li "bystanders.note"
      li "setAside"
      li.nested "sickeningReality.instructions" do
        li "sickeningReality.note"
      unscoped $ li "shuffleRemainder"

  whenReturnTo $ gather Set.ReturnToTheLastKing
  gather Set.TheLastKing
  gather Set.HastursGift
  gather Set.DecayAndFilth `orWhenReturnTo` gather Set.DecayingReality
  gather Set.TheStranger
  gather Set.AncientEvils `orWhenReturnTo` gather Set.DelusoryEvils

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

  isReturnTo <- getIsReturnTo

  setAside [Enemies.dianneDevine]
    `orWhenReturnTo` setAside [Assets.dianneDevineHidingAnOathUnspoken, Treacheries.shockingDisplay]

  whenReturnTo $ removeEvery [Enemies.dianneDevine]

  placeUnderScenarioReference
    $ [ Story.sickeningReality_65
      , Story.sickeningReality_66
      , Story.sickeningReality_67
      , Story.sickeningReality_68
      , Story.sickeningReality_69
      ]
    <> ( guard isReturnTo
           *> [ Story.returnToSickeningReality_23
              , Story.returnToSickeningReality_24
              , Story.returnToSickeningReality_24
              ]
       )

  setAgendaDeck
    [ if isReturnTo then Agendas.betterNeverThanLate else Agendas.fashionablyLate
    , Agendas.theTerrifyingTruth
    ]
  setActDeck [Acts.discoveringTheTruth]

instance RunMessage TheLastKing where
  runMessage msg s@(TheLastKing attrs) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      flavor $ scope "intro" $ h "title" >> p "body"
      pure s
    StandaloneSetup -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      setChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
      lead <- getLead
      addCampaignCardToDeck lead ShuffleIn Enemies.theManInThePallidMask
      pure s
    Setup -> runScenarioSetup TheLastKing attrs $ setupTheLastKing attrs
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
    ScenarioResolution r -> scope "resolutions" do
      case r of
        NoResolution -> do
          anyResigned <- notNull <$> select ResignedInvestigator
          flavor do
            p.validate anyResigned "goToR1"
            p.validate (not anyResigned) "goToR2"
          do_ $ if anyResigned then R1 else R2
        _ -> do_ msg
      pure s
    Do msg'@(ScenarioResolution r) -> scope "resolutions" do
      let interviewed = mapMaybe interviewedToCardCode (setToList attrs.log)
      when (notNull interviewed) $ recordSetInsert VIPsInterviewed interviewed
      vipsSlain <- selectMap toCardCode $ VictoryDisplayCardMatch $ basic $ CardWithTrait Trait.Lunatic
      when (notNull vipsSlain) $ recordSetInsert VIPsSlain vipsSlain

      let
        handleExtraXp = do
          -- Resolution handles XP in a special way, we must divvy up between investigators
          -- evenly and apply, this will have a weird interaction with Hospital Debts so we
          -- want to handle `getXp` in two phases. The first phase will essentially evenly
          -- add XP modifiers to the players in order to have `getXp` resolve "normally"
          clueCounts <- traverse (field ActClues) =<< select AnyAct
          investigators <- allInvestigators
          let
            extraXp = floor @Double (fromIntegral (sum clueCounts) / 2)
            (assignedXp, remainingXp) = quotRem extraXp (length investigators)
            assignXp :: ReverseQueue m => Int -> InvestigatorId -> m ()
            assignXp amount iid =
              resolutionModifier attrs iid
                $ XPModifier ("$" <> ikey "xp.bonus") amount
          for_ investigators (assignXp assignedXp)

          when (remainingXp > 0) $ popScope do
            lead <- getLead
            chooseNM lead remainingXp do
              for_ investigators \iid -> do
                name <- field InvestigatorName iid
                withVar "name" (String $ display name) $ labeled' "gainAdditionalXp" $ assignXp 1 iid

          -- Assign XP now that the modifiers exist
          doStep 1 msg'
        adjustTokens = do
          removeAllChaosTokens Cultist
          removeAllChaosTokens Tablet
          removeAllChaosTokens ElderThing
          addChaosToken Cultist
          addChaosToken Tablet
          addChaosToken ElderThing

      case r of
        Resolution 1 -> do
          resolutionWithXp "resolution1" (fst <$> getXp')
          handleExtraXp
          endOfScenarioThen $ InterludeStep 1 Nothing
        Resolution 2 -> do
          adjustTokens
          resolutionWithXp "resolution2" (fst <$> getXp')
          handleExtraXp
          endOfScenario
        Resolution 3 -> do
          crossOutRecordSetEntries VIPsInterviewed interviewed
          adjustTokens
          resolutionWithXp "resolution3" (fst <$> getXp')
          handleExtraXp
          endOfScenario
        _ -> throw $ UnknownResolution r
      pure s
    DoStep 1 (ScenarioResolution _) -> do
      allGainXp attrs
      pure s
    _ -> TheLastKing <$> liftRunMessage msg attrs
