module Arkham.Scenario.Scenarios.TheLastKing (
  TheLastKing (..),
  theLastKing,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Types (Field (..))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Name
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
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
      lid <- getJustLocation iid
      shroud <- fieldJust LocationShroud lid
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
  runMessage msg s@(TheLastKing attrs) = case msg of
    SetChaosTokensForScenario -> do
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      whenStandalone
        $ push (SetChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken])
      pure s
    StandaloneSetup -> do
      lead <- getLead
      push $ AddCampaignCardToDeck lead Enemies.theManInThePallidMask
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.dianneDevine]
          [ EncounterSet.TheLastKing
          , EncounterSet.HastursGift
          , EncounterSet.DecayAndFilth
          , EncounterSet.TheStranger
          , EncounterSet.AncientEvils
          ]

      (foyerId, placeFoyer) <- placeLocationCard Locations.foyer
      otherPlacements <-
        traverse
          placeLocationCard
          [ Locations.courtyard
          , Locations.livingRoom
          , Locations.ballroom
          , Locations.diningRoom
          , Locations.gallery
          ]

      totalClues <- getPlayerCountValue (StaticWithPerPlayer 1 1)

      bystanders <-
        traverse (\c -> (c,) <$> getRandom)
          =<< shuffleM
          =<< genCards
            [ Assets.constanceDumaine
            , Assets.jordanPerry
            , Assets.ishimaruHaruko
            , Assets.sebastienMoreau
            , Assets.ashleighClarke
            ]

      destinations <- shuffleM $ map fst otherPlacements
      players <- allPlayers

      pushAll
        $ [ story players intro
          , SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeFoyer
          ]
        <> map snd otherPlacements
        <> [MoveAllTo (toSource attrs) foyerId]
        <> zipWith
          ( \(bystander, assetId) placement ->
              CreateAssetAt assetId bystander placement
          )
          bystanders
          (map AtLocation destinations)
        <> [ PlaceTokens (toSource attrs) (toTarget assetId) Clue totalClues
           | (_, assetId) <- bystanders
           ]

      setAsideEncounterCards <- genCards [Enemies.dianneDevine]

      storyCards <-
        genCards
          [ Story.sickeningReality_65
          , Story.sickeningReality_66
          , Story.sickeningReality_67
          , Story.sickeningReality_68
          , Story.sickeningReality_69
          ]
      agendas <- genCards [Agendas.fashionablyLate, Agendas.theTerrifyingTruth]
      acts <- genCards [Acts.discoveringTheTruth]

      TheLastKing
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideEncounterCards)
              & (cardsUnderScenarioReferenceL .~ storyCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ token iid | token `elem` [Skull, Cultist, Tablet] -> do
      case token of
        Skull -> push $ DrawAnotherChaosToken iid
        Cultist | isHardExpert attrs -> do
          clueCount <- field InvestigatorClues iid
          when
            (clueCount > 0)
            (push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Cultist) 1)
        Tablet | isHardExpert attrs -> do
          push $ InvestigatorAssignDamage iid (ChaosTokenEffectSource token) DamageAny 0 1
        _ -> pure ()
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case chaosTokenFace token of
        Skull -> do
          targets <-
            selectMap EnemyTarget
              $ if isEasyStandard attrs
                then EnemyWithTrait Trait.Lunatic
                else EnemyWithMostRemainingHealth $ EnemyWithTrait Trait.Lunatic
          player <- getPlayer iid
          when (notNull targets) $ do
            push
              $ chooseOrRunOne
                player
                [targetLabel target [PlaceTokens (ChaosTokenEffectSource Skull) target Doom 1] | target <- targets]
        Cultist | isEasyStandard attrs -> do
          clueCount <- field InvestigatorClues iid
          when
            (clueCount > 0)
            (push $ InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Cultist) 1)
        Tablet | isEasyStandard attrs -> do
          push $ InvestigatorAssignDamage iid (ChaosTokenSource token) DamageAny 0 1
        ElderThing | isHardExpert attrs -> do
          push $ InvestigatorAssignDamage iid (ChaosTokenSource token) DamageAny 1 0
        _ -> pure ()
      pure s
    ScenarioResolution NoResolution -> do
      anyResigned <- notNull <$> select ResignedInvestigator
      s <$ push (ScenarioResolution $ Resolution $ if anyResigned then 1 else 2)
    ScenarioResolution (Resolution n) -> do
      -- Resolution handles XP in a special way, we must divvy up between investigators
      -- evenly and apply, this will have a weird interaction with Hospital Debts so we
      -- want to handle `getXp` in two phases. The first phase will essentially evenly
      -- add XP modifiers to the players in order to have `getXp` resolve "normally"
      investigatorIds <- allInvestigatorIds
      investigatorIdsWithNames <-
        forToSnd
          investigatorIds
          (field InvestigatorName)
      lead <- getLeadPlayer
      clueCounts <- traverse (field ActClues) =<< select AnyAct
      vipsSlain <-
        selectMap toCardCode
          $ VictoryDisplayCardMatch
          $ CardWithTrait
            Trait.Lunatic
      let
        interviewed =
          mapMaybe interviewedToCardCode (setToList $ scenarioLog attrs)
        extraXp = ceiling @Double (fromIntegral (sum clueCounts) / 2)
        (assignedXp, remainingXp) = quotRem extraXp (length investigatorIds)
        assignXp amount iid =
          CreateWindowModifierEffect
            EffectGameWindow
            (EffectModifiers $ toModifiers (toSource attrs) [XPModifier amount])
            (toSource attrs)
            (InvestigatorTarget iid)
      pushAll
        $ [assignXp assignedXp iid | iid <- investigatorIds]
        <> [ chooseN
              lead
              remainingXp
              [ Label ("Choose " <> display name <> " to gain 1 additional XP") [assignXp 1 iid]
              | (iid, name) <- investigatorIdsWithNames
              ]
           ]
        <> [recordSetInsert VIPsInterviewed interviewed | notNull interviewed]
        <> [recordSetInsert VIPsSlain vipsSlain | notNull vipsSlain]
        <> ( if n == 2 || n == 3
              then
                [ RemoveAllChaosTokens Cultist
                , RemoveAllChaosTokens Tablet
                , RemoveAllChaosTokens ElderThing
                , AddChaosToken Cultist
                , AddChaosToken Tablet
                , AddChaosToken ElderThing
                ]
              else []
           )
        <> [crossOutRecordSetEntries VIPsInterviewed interviewed | n == 3]
        <> [ScenarioResolutionStep 1 (Resolution n)]
      pure s
    ScenarioResolutionStep 1 (Resolution n) -> do
      players <- allPlayers
      gainXp <- toGainXp attrs getXp
      case n of
        1 ->
          pushAll
            $ [story players resolution1]
            <> gainXp
            <> [EndOfGame (Just $ InterludeStep 1 Nothing)]
        2 ->
          pushAll
            $ [story players resolution2]
            <> gainXp
            <> [EndOfGame Nothing]
        3 ->
          pushAll
            $ [story players resolution3]
            <> gainXp
            <> [EndOfGame Nothing]
        _ -> error "Invalid resolution"
      pure s
    _ -> TheLastKing <$> runMessage msg attrs
