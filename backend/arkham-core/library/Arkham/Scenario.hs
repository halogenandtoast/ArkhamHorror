{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenario (
  module Arkham.Scenario,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Uses
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Damage
import Arkham.Deck qualified as Deck
import Arkham.Difficulty
import Arkham.EncounterSet (EncounterSet)
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Investigator qualified as Helpers
import Arkham.Helpers.SkillTest
import Arkham.Helpers.Tarot
import Arkham.History
import Arkham.Id
import Arkham.Investigator.Types qualified as Field
import Arkham.Matcher qualified as Matcher
import Arkham.Message
import Arkham.Name
import Arkham.Projection
import Arkham.Scenario.Runner
import Arkham.Scenario.Scenarios
import Arkham.Tarot
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Window (duringTurnWindow)
import Arkham.Window qualified as Window
import Data.Map.Strict qualified as Map

instance FromJSON Scenario where
  parseJSON = withObject "Scenario" $ \o -> do
    cCode <- o .: "id"
    case lookup cCode allScenarios of
      Nothing -> error $ "Unknown scenario: " <> show cCode
      Just (SomeScenario (_ :: Difficulty -> a)) ->
        Scenario <$> parseJSON @a (Object o)

instance HasAbilities Scenario where
  getAbilities (Scenario x) = concatMap getAbilities $ concat $ toList (attr scenarioTarotCards x)

instance HasAbilities TarotCard where
  getAbilities c@(TarotCard facing arcana) = case arcana of
    TheLoversVI ->
      [ restrictedAbility (TarotSource c) 1 AffectedByTarot
          $ ForcedAbility (Matcher.GameBegins #when)
      ]
    StrengthVIII | facing == Upright -> do
      [restrictedAbility (TarotSource c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    WheelOfFortuneX -> case facing of
      Upright ->
        [ restrictedAbility (TarotSource c) 1 (AffectedByTarot <> ActExists Matcher.ActCanWheelOfFortuneX)
            $ ReactionAbility (Matcher.RevealChaosToken #when Matcher.You #autofail) Free
        ]
      Reversed ->
        [ restrictedAbility
            (TarotSource c)
            1
            (AffectedByTarot <> AgendaExists Matcher.AgendaCanWheelOfFortuneX)
            $ ForcedAbility (Matcher.RevealChaosToken #when Matcher.You #eldersign)
        ]
    JusticeXI -> case facing of
      Upright ->
        [ groupLimit PerGame
            $ restrictedAbility (TarotSource c) 1 AffectedByTarot
            $ ForcedAbility
              ( Matcher.WouldPlaceDoomCounter
                  #when
                  Matcher.AnySource
                  (Matcher.AgendaTargetMatches Matcher.FinalAgenda)
              )
        ]
      Reversed ->
        [ restrictedAbility (TarotSource c) 1 AffectedByTarot
            $ ForcedAbility (Matcher.AgendaEntersPlay #when Matcher.FinalAgenda)
        ]
    TheDevilXV ->
      [restrictedAbility (TarotSource c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    TheTowerXVI -> do
      -- This is handled by SetupInvestigators below
      [restrictedAbility (TarotSource c) 1 AffectedByTarot $ ForcedAbility Matcher.NotAnyWindow]
    TheStarXVII -> case facing of
      Upright ->
        [ restrictedAbility
            (TarotSource c)
            1
            ( AffectedByTarot
                <> DuringSkillTest Matcher.AnySkillTest
                <> InvestigatorExists
                  ( Matcher.AnyInvestigator
                      [ Matcher.HealableInvestigator (TarotSource c) DamageType Matcher.You
                      , Matcher.HealableInvestigator (TarotSource c) HorrorType Matcher.You
                      ]
                  )
            )
            $ ForcedAbility (Matcher.RevealChaosToken #when Matcher.You #eldersign)
        ]
      Reversed ->
        [ restrictedAbility
            (TarotSource c)
            1
            (AffectedByTarot <> DuringSkillTest Matcher.AnySkillTest)
            $ ForcedAbility (Matcher.RevealChaosToken #when Matcher.You #autofail)
        ]
    TheMoonXVIII -> case facing of
      Upright ->
        [ playerLimit PerGame
            $ restrictedAbility (TarotSource c) 1 AffectedByTarot
            $ ForcedAbility (Matcher.DeckWouldRunOutOfCards #when Matcher.You)
        ]
      Reversed -> [restrictedAbility (TarotSource c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    JudgementXX ->
      [restrictedAbility (TarotSource c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    TheWorldXXI ->
      [restrictedAbility (TarotSource c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameEnds #when)]
    _ -> []

tarotInvestigator :: HasGame m => TarotCard -> m (Maybe InvestigatorId)
tarotInvestigator card = do
  tarotCards <- Map.assocs <$> scenarioField ScenarioTarotCards
  pure $ case find (\(_, vs) -> card `elem` vs) tarotCards of
    Nothing -> Nothing
    Just (InvestigatorTarot iid, _) -> Just iid
    Just (GlobalTarot, _) -> Nothing

instance HasModifiersFor TarotCard where
  getModifiersFor target c@(TarotCard facing arcana) = do
    let source = TarotSource c
    case arcana of
      TheFool0 ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            defeated <- iid <=~> Matcher.DefeatedInvestigator
            pure
              . toModifiers source
              $ case facing of
                Upright -> [XPModifier 2 | not defeated && affected]
                Reversed -> [XPModifier (-2) | defeated && affected]
          _ -> pure []
      TheMagicianI ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            firstTurn <- scenarioFieldMap ScenarioTurn (== 1)
            pure
              . map setActiveDuringSetup
              . toModifiers source
              $ case facing of
                Upright -> [StartingResources 3 | affected]
                Reversed -> guard affected *> (StartingResources (-3) : [CannotGainResources | firstTurn])
          _ -> pure []
      TheHighPriestessII ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            history <- getHistory TurnHistory iid
            currentSkillTypes <- getSkillTestSkillTypes
            let
              skillTypes = concat $ historySkillTestsPerformed history
              firstIntellectTest = #intellect `notElem` skillTypes && #intellect `elem` currentSkillTypes
            pure
              . toModifiers source
              $ case facing of
                Upright -> [SkillModifier #intellect 1 | firstIntellectTest && affected]
                Reversed -> [SkillModifier #intellect (-1) | firstIntellectTest && affected]
          _ -> pure []
      TheEmpressIII ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            history <- getHistory TurnHistory iid
            currentSkillTypes <- getSkillTestSkillTypes
            let
              skillTypes = concat $ historySkillTestsPerformed history
              firstAgilityTest = #agility `notElem` skillTypes && #agility `elem` currentSkillTypes
            pure
              . toModifiers source
              $ case facing of
                Upright -> [SkillModifier #agility 1 | firstAgilityTest && affected]
                Reversed -> [SkillModifier #agility (-1) | firstAgilityTest && affected]
          _ -> pure []
      TheEmperorIV ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            history <- getHistory TurnHistory iid
            currentSkillTypes <- getSkillTestSkillTypes
            let
              skillTypes = concat $ historySkillTestsPerformed history
              firstCombatTest = #combat `notElem` skillTypes && #combat `elem` currentSkillTypes
            pure
              . toModifiers source
              $ case facing of
                Upright -> [SkillModifier #combat 1 | firstCombatTest && affected]
                Reversed -> [SkillModifier #combat (-1) | firstCombatTest && affected]
          _ -> pure []
      TheHierophantV ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            history <- getHistory TurnHistory iid
            currentSkillTypes <- getSkillTestSkillTypes
            let
              skillTypes = concat $ historySkillTestsPerformed history
              firstWillpowerTest = #willpower `notElem` skillTypes && #willpower `elem` currentSkillTypes
            pure
              . toModifiers source
              $ case facing of
                Upright -> [SkillModifier #willpower 1 | firstWillpowerTest && affected]
                Reversed -> [SkillModifier #willpower (-1) | firstWillpowerTest && affected]
          _ -> pure []
      TheLoversVI ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheChariotVII ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            firstTurn <- scenarioFieldMap ScenarioTurn (== 1)
            pure
              . map setActiveDuringSetup
              . toModifiers source
              $ case facing of
                Upright -> [StartingHand 2 | affected]
                Reversed -> guard affected *> (StartingHand (-2) : [CannotDrawCards | firstTurn])
          _ -> pure []
      StrengthVIII ->
        case facing of
          Upright -> pure []
          Reversed -> do
            case target of
              InvestigatorTarget iid -> do
                affected <- affectedByTarot iid c
                firstTurn <- scenarioFieldMap ScenarioTurn (== 1)
                pure $ toModifiers source [CannotPlay #asset | firstTurn && affected]
              _ -> pure []
      TheHermitIX ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            pure
              . toModifiers source
              $ case facing of
                Upright -> [HandSize 3 | affected]
                Reversed -> [HandSize (-3) | affected]
          _ -> pure []
      WheelOfFortuneX ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      JusticeXI ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheHangedManXII ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            pure
              . map setActiveDuringSetup
              . toModifiers source
              $ case facing of
                Upright -> [Mulligans 2 | affected]
                Reversed -> guard affected *> [CannotMulligan, CannotReplaceWeaknesses]
          _ -> pure []
      DeathXIII ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            pure
              . toModifiers source
              $ case facing of
                Upright -> [HealthModifier 1 | affected]
                Reversed -> [HealthModifier (-1) | affected]
          _ -> pure []
      TemperanceXIV ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            pure
              . toModifiers source
              $ case facing of
                Upright -> [SanityModifier 1 | affected]
                Reversed -> [SanityModifier (-1) | affected]
          _ -> pure []
      TheDevilXV ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheTowerXVI ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheStarXVII ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheMoonXVIII ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheSunXIX ->
        case target of
          InvestigatorTarget iid -> do
            affected <- affectedByTarot iid c
            firstTurn <- scenarioFieldMap ScenarioTurn (== 1)
            pure
              . toModifiers source
              $ case facing of
                Upright -> [AdditionalActions 2 | firstTurn && affected]
                Reversed -> [FewerActions 2 | firstTurn && affected]
          _ -> pure []
      JudgementXX ->
        case facing of
          Upright -> pure []
          Reversed -> pure []
      TheWorldXXI ->
        case facing of
          Upright -> pure []
          Reversed -> pure []

instance HasModifiersFor Scenario where
  getModifiersFor target (Scenario a) =
    liftA2
      (<>)
      (concatMapM (getModifiersFor target) (concat . toList $ attr scenarioTarotCards a))
      (getModifiersFor target a)

instance RunMessage Scenario where
  runMessage msg x@(Scenario s) = case msg of
    UseThisAbility _ source@(TarotSource card@(TarotCard facing TheLoversVI)) 1 -> do
      investigators <- filterM (`affectedByTarot` card) =<< getInvestigators
      pushAll
        [ search
          iid
          source
          iid
          [fromDeck]
          (#asset <> #ally)
          (if facing == Upright then DrawFound iid 1 else RemoveFoundFromGame iid 1)
        | iid <- investigators
        ]
      pure x
    UseThisAbility _ source@(TarotSource card@(TarotCard Upright StrengthVIII)) 1 -> do
      investigators <- filterM (`affectedByTarot` card) =<< getInvestigators
      msgs <- forMaybeM investigators $ \investigator -> do
        results <- selectList (Matcher.InHandOf (Matcher.InvestigatorWithId investigator) <> #asset)
        resources <- getSpendableResources investigator
        cards <-
          filterM
            ( getIsPlayableWithResources
                investigator
                source
                (resources + 2)
                UnpaidCost
                [duringTurnWindow investigator]
            )
            results
        pure
          $ Just
          $ chooseOne investigator
          $ Label "Do not play asset" []
          : [ targetLabel
              (toCardId c)
              [ costModifier source investigator (ReduceCostOf (Matcher.CardWithId $ toCardId c) 2)
              , PayCardCost investigator c [duringTurnWindow investigator]
              ]
            | c <- cards
            ]

      pushAll msgs
      pure x
    UseCardAbility _ source@(TarotSource card@(TarotCard facing JusticeXI)) 1 ws _ -> do
      case facing of
        Upright -> do
          let
            getDoomTarget [] = error "wrong window"
            getDoomTarget ((Window.windowType -> Window.WouldPlaceDoom _ doomTarget _) : _) = doomTarget
            getDoomTarget (_ : xs) = getDoomTarget xs
            target = getDoomTarget ws
          cancelDoom target 1
        Reversed -> do
          mInvestigator <- tarotInvestigator card
          lead <- getLead
          let investigator = fromMaybe lead mInvestigator

          agendas <- selectList Matcher.AnyAgenda
          push
            $ chooseOrRunOne
              investigator
              [targetLabel agenda [PlaceDoom source (toTarget agenda) 1] | agenda <- agendas]
      -- cancelDoom 1
      pure x
    UseThisAbility _ source@(TarotSource card@(TarotCard facing TheDevilXV)) 1 -> do
      investigators <- filterM (`affectedByTarot` card) =<< getInvestigators
      case facing of
        Upright -> do
          pushAll
            [ chooseOne
              investigator
              [ Label ("Add " <> slotName slotType <> " Slot") [AddSlot investigator slotType (Slot source [])]
              | slotType <- allSlotTypes
              ]
            | investigator <- investigators
            ]
        Reversed -> do
          for_ investigators $ \investigator -> do
            slotTypes <- keys . filterMap notNull <$> field Field.InvestigatorSlots investigator
            pushWhen (notNull slotTypes)
              $ chooseN
                investigator
                (min 3 $ length slotTypes)
                [ Label ("Remove " <> slotName slotType <> " Slot") [RemoveSlot investigator slotType]
                | slotType <- slotTypes
                ]
      pure x
    UseThisAbility _ source@(TarotSource card@(TarotCard facing TheTowerXVI)) 1 -> do
      investigators <- filterM (`affectedByTarot` card) =<< getInvestigators
      case facing of
        Upright ->
          pushAll
            [ search
              iid
              source
              iid
              [fromDeck]
              (Matcher.CardWithSubType BasicWeakness)
              (RemoveFoundFromGame iid 1)
            | iid <- investigators
            ]
        Reversed ->
          for_ investigators $ \investigator ->
            push
              $ SearchCollectionForRandom investigator source
              $ Matcher.CardWithSubType BasicWeakness
      pure x
    RequestedPlayerCard iid (TarotSource (TarotCard Reversed TheTowerXVI)) (Just c) _ -> do
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [toCard c]
      pure x
    UseThisAbility iid source@(TarotSource (TarotCard facing TheStarXVII)) 1 -> do
      case facing of
        Upright -> do
          canHealDamage <- Helpers.canHaveDamageHealed source iid
          mHealHorror <- Helpers.getHealHorrorMessage source 1 iid
          push
            $ chooseOrRunOne iid
            $ [DamageLabel iid [HealDamage (toTarget iid) source 1] | canHealDamage]
            <> [HorrorLabel iid [healHorror] | healHorror <- toList mHealHorror]
        Reversed ->
          push
            $ chooseOne
              iid
              [ Label "Take 1 damage" [assignDamage iid source 1]
              , Label "Take 1 horror" [assignHorror iid source 1]
              ]
      pure x
    UseCardAbility
      iid
      (TarotSource (TarotCard Upright TheMoonXVIII))
      1
      (Window.getBatchId -> batchId)
      _ -> do
        let
          getDraw [] = error "no drawing"
          getDraw (Would batchId' msgs : _) | batchId' == batchId = getDraw msgs
          getDraw (Do (EmptyDeck _ (Just draw)) : _) = draw
          getDraw (_ : rest) = getDraw rest
        drawing <- fromQueue getDraw
        cards <- map toCard . take 10 . reverse <$> field Field.InvestigatorDiscard iid
        push
          $ chooseOne
            iid
            [ Label
                "Shuffle the bottom 10 cards of your discard back into your Deck"
                [IgnoreBatch batchId, ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards, drawing]
            , Label "Do Nothing" []
            ]
        pure x
    UseThisAbility _ source@(TarotSource card@(TarotCard Reversed TheMoonXVIII)) 1 -> do
      investigators <- filterM (`affectedByTarot` card) =<< getInvestigators
      for_ investigators $ \investigator -> do
        push $ DiscardTopOfDeck investigator 5 source (Just $ TarotTarget (TarotCard Reversed TheMoonXVIII))
      pure x
    DiscardedTopOfDeck iid cards _ (TarotTarget (TarotCard Reversed TheMoonXVIII)) -> do
      let weaknesses = filter (`cardMatch` Matcher.WeaknessCard) cards
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) (map toCard weaknesses)
      pure x
    UseThisAbility _ (TarotSource (TarotCard facing JudgementXX)) 1 -> do
      case facing of
        Upright -> push $ SwapChaosToken Skull Zero
        Reversed -> do
          tokenFaces <- filter isNonNegativeChaosToken . map chaosTokenFace <$> getBagChaosTokens
          case nonEmpty tokenFaces of
            Just (face :| faces) -> do
              let maxFace =
                    foldr
                      ( \f g ->
                          if chaosTokenToFaceValue f > chaosTokenToFaceValue g then f else g
                      )
                      face
                      faces
              push $ SwapChaosToken maxFace Skull
            Nothing -> pure ()
      pure x
    UseThisAbility _ (TarotSource card@(TarotCard facing TheWorldXXI)) 1 -> do
      case facing of
        Upright -> do
          investigators <- filterM (`affectedByTarot` card) =<< getInvestigators
          investigatorsWhoCanHealTrauma <-
            catMaybes <$> for
              investigators
              \iid -> do
                hasPhysicalTrauma <- fieldP Field.InvestigatorPhysicalTrauma (> 0) iid
                hasMentalTrauma <- fieldP Field.InvestigatorMentalTrauma (> 0) iid
                if (hasPhysicalTrauma || hasMentalTrauma)
                  then pure $ Just (iid, hasPhysicalTrauma, hasMentalTrauma)
                  else pure Nothing

          pushAll
            $ [ chooseOne iid
                $ [Label "Remove a physical trauma" [HealTrauma iid 1 0] | hasPhysical]
                <> [Label "Remove a mental trauma" [HealTrauma iid 0 1] | hasMental]
                <> [Label "Do not remove trauma" []]
              | (iid, hasPhysical, hasMental) <- investigatorsWhoCanHealTrauma
              ]
        Reversed -> do
          defeatedInvestigators <-
            filterM (`affectedByTarot` card) =<< selectList Matcher.DefeatedInvestigator
          pushAll
            $ [ chooseOne
                iid
                [ Label "Suffer physical trauma" [SufferTrauma iid 1 0]
                , Label "Suffer mental trauma" [SufferTrauma iid 0 1]
                ]
              | iid <- defeatedInvestigators
              ]
      pure x
    ResolveChaosToken _ chaosTokenFace _ -> do
      modifiers' <- getModifiers (ChaosTokenFaceTarget chaosTokenFace)
      if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
        then pure x
        else go
    FailedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      modifiers' <- getModifiers (ChaosTokenFaceTarget $ chaosTokenFace token)
      if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
        then pure x
        else go
    PassedSkillTest _ _ _ (ChaosTokenTarget token) _ _ -> do
      modifiers' <- getModifiers (ChaosTokenFaceTarget $ chaosTokenFace token)
      if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
        then pure x
        else go
    SetupInvestigators -> do
      result <- go
      let isTowerXVI = (== TheTowerXVI) . toTarotArcana
      for_ (filter isTowerXVI $ concat $ toList $ attr scenarioTarotCards s) $ \card -> do
        lead <- getLead
        mInvestigator <- tarotInvestigator card
        let investigator = fromMaybe lead mInvestigator
        let abilities = getAbilities card
        for_ abilities $ \ability -> do
          push $ chooseOne investigator [AbilityLabel investigator ability [] []]
      pure result
    PreScenarioSetup -> do
      result <- go
      observed <- selectList $ Matcher.DeckWith $ Matcher.HasCard $ Matcher.cardIs Assets.observed4
      for_ observed $ \iid -> do
        push $ DrawAndChooseTarot iid Upright 3
      damned <- selectList $ Matcher.DeckWith $ Matcher.HasCard $ Matcher.cardIs Treacheries.damned
      for_ damned $ \iid -> do
        push $ DrawAndChooseTarot iid Reversed 1
      pure result
    _ -> go
   where
    go = Scenario <$> runMessage msg s

instance HasChaosTokenValue Scenario where
  getChaosTokenValue iid chaosTokenFace (Scenario s) = do
    modifiers' <- getModifiers (ChaosTokenFaceTarget chaosTokenFace)
    if any (`elem` modifiers') [IgnoreChaosTokenEffects, IgnoreChaosToken]
      then pure $ ChaosTokenValue chaosTokenFace NoModifier
      else getChaosTokenValue iid chaosTokenFace s

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario scenarioId =
  case lookup (unScenarioId scenarioId) allScenarios of
    Nothing -> error $ "Unknown scenario: " <> show scenarioId
    Just (SomeScenario f) -> Scenario . f

data SomeScenario = forall a. IsScenario a => SomeScenario (Difficulty -> a)

scenarioCard :: CardCode -> Name -> EncounterSet -> CardDef
scenarioCard cCode name ecSet =
  CardDef
    { cdCardCode = cCode
    , cdName = name
    , cdRevealedName = Nothing
    , cdCost = Nothing
    , cdAdditionalCost = Nothing
    , cdLevel = 0
    , cdCardType = ScenarioType
    , cdCardSubType = Nothing
    , cdClassSymbols = mempty
    , cdSkills = mempty
    , cdCardTraits = mempty
    , cdRevealedCardTraits = mempty
    , cdKeywords = mempty
    , cdFastWindow = Nothing
    , cdActions = mempty
    , cdRevelation = NoRevelation
    , cdVictoryPoints = Nothing
    , cdVengeancePoints = Nothing
    , cdCriteria = Nothing
    , cdOverrideActionPlayableIfCriteriaMet = False
    , cdCommitRestrictions = mempty
    , cdAttackOfOpportunityModifiers = mempty
    , cdPermanent = False
    , cdEncounterSet = Just ecSet
    , cdEncounterSetQuantity = Nothing
    , cdUnique = True
    , cdDoubleSided = True
    , cdLimits = []
    , cdExceptional = False
    , cdUses = NoUses
    , cdPlayableFromDiscard = False
    , cdStage = Nothing
    , cdSlots = mempty
    , cdCardInHandEffects = False
    , cdCardInDiscardEffects = False
    , cdCardInSearchEffects = False
    , cdAlternateCardCodes = mempty
    , cdArt = unCardCode cCode
    , cdLocationSymbol = Nothing
    , cdLocationRevealedSymbol = Nothing
    , cdLocationConnections = mempty
    , cdLocationRevealedConnections = mempty
    , cdPurchaseMentalTrauma = Nothing
    , cdGrantedXp = Nothing
    , cdCanReplace = True
    , cdDeckRestrictions = []
    }

allScenarioCards :: Map CardCode CardDef
allScenarioCards =
  mapFromList $ flip map (mapToList allScenarios) $ \(c, SomeScenario s) -> do
    let ecSet = fromJustNote "you forgot to add the encounter set" $ lookup c scenarioEncounterSets
        name = scenarioName $ toAttrs $ Scenario (s Easy)
    (c, scenarioCard c name ecSet)

allScenarios :: Map CardCode SomeScenario
allScenarios =
  mapFromList
    [ ("01104", SomeScenario theGathering)
    , ("01120", SomeScenario theMidnightMasks)
    , ("01142", SomeScenario theDevourerBelow)
    , ("02041", SomeScenario extracurricularActivity)
    , ("02062", SomeScenario theHouseAlwaysWins)
    , ("02118", SomeScenario theMiskatonicMuseum)
    , ("02159", SomeScenario theEssexCountyExpress)
    , ("02195", SomeScenario bloodOnTheAltar)
    , ("02236", SomeScenario undimensionedAndUnseen)
    , ("02274", SomeScenario whereDoomAwaits)
    , ("02311", SomeScenario lostInTimeAndSpace)
    , ("03043", SomeScenario curtainCall)
    , ("03061", SomeScenario theLastKing)
    , ("03120", SomeScenario echoesOfThePast)
    , ("03159", SomeScenario theUnspeakableOath)
    , ("03200", SomeScenario aPhantomOfTruth)
    , ("03240", SomeScenario thePallidMask)
    , ("03274", SomeScenario blackStarsRise)
    , ("03316", SomeScenario dimCarcosa)
    , ("04043", SomeScenario theUntamedWilds)
    , ("04054", SomeScenario theDoomOfEztli)
    , ("04113", SomeScenario threadsOfFate)
    , ("04161", SomeScenario theBoundaryBeyond)
    , ("04205", SomeScenario heartOfTheElders)
    , ("04237", SomeScenario theCityOfArchives)
    , ("04277", SomeScenario theDepthsOfYoth)
    , ("04314", SomeScenario shatteredAeons)
    , ("04344", SomeScenario turnBackTime)
    , ("05043", SomeScenario disappearanceAtTheTwilightEstate)
    , ("05050", SomeScenario theWitchingHour)
    , ("05065", SomeScenario atDeathsDoorstep)
    , ("05120", SomeScenario theSecretName)
    , ("05161", SomeScenario theWagesOfSin)
    , ("05197", SomeScenario forTheGreaterGood)
    , ("05238", SomeScenario unionAndDisillusion)
    , ("05284", SomeScenario inTheClutchesOfChaos)
    , ("05325", SomeScenario beforeTheBlackThrone)
    , ("50011", SomeScenario returnToTheGathering)
    , ("50025", SomeScenario returnToTheMidnightMasks)
    , ("50032", SomeScenario returnToTheDevourerBelow)
    , ("81001", SomeScenario curseOfTheRougarou)
    , ("82001", SomeScenario carnevaleOfHorrors)
    ]

scenarioEncounterSets :: Map CardCode EncounterSet
scenarioEncounterSets =
  mapFromList
    [ ("01104", EncounterSet.TheGathering)
    , ("01120", EncounterSet.TheMidnightMasks)
    , ("01142", EncounterSet.TheDevourerBelow)
    , ("02041", EncounterSet.ExtracurricularActivity)
    , ("02062", EncounterSet.TheHouseAlwaysWins)
    , ("02118", EncounterSet.TheMiskatonicMuseum)
    , ("02159", EncounterSet.TheEssexCountyExpress)
    , ("02195", EncounterSet.BloodOnTheAltar)
    , ("02236", EncounterSet.UndimensionedAndUnseen)
    , ("02274", EncounterSet.WhereDoomAwaits)
    , ("02311", EncounterSet.LostInTimeAndSpace)
    , ("03043", EncounterSet.CurtainCall)
    , ("03061", EncounterSet.TheLastKing)
    , ("03120", EncounterSet.EchoesOfThePast)
    , ("03159", EncounterSet.TheUnspeakableOath)
    , ("03200", EncounterSet.APhantomOfTruth)
    , ("03240", EncounterSet.ThePallidMask)
    , ("03274", EncounterSet.BlackStarsRise)
    , ("03316", EncounterSet.DimCarcosa)
    , ("04043", EncounterSet.TheUntamedWilds)
    , ("04054", EncounterSet.TheDoomOfEztli)
    , ("04113", EncounterSet.ThreadsOfFate)
    , ("04161", EncounterSet.TheBoundaryBeyond)
    , ("04205", EncounterSet.HeartOfTheElders)
    , ("04237", EncounterSet.TheCityOfArchives)
    , ("04277", EncounterSet.TheDepthsOfYoth)
    , ("04314", EncounterSet.ShatteredAeons)
    , ("04344", EncounterSet.TurnBackTime)
    , ("05043", EncounterSet.DisappearanceAtTheTwilightEstate)
    , ("05050", EncounterSet.TheWitchingHour)
    , ("05065", EncounterSet.AtDeathsDoorstep)
    , ("05120", EncounterSet.TheSecretName)
    , ("05161", EncounterSet.TheWagesOfSin)
    , ("05197", EncounterSet.ForTheGreaterGood)
    , ("05238", EncounterSet.UnionAndDisillusion)
    , ("05284", EncounterSet.InTheClutchesOfChaos)
    , ("05325", EncounterSet.BeforeTheBlackThrone)
    , ("50011", EncounterSet.ReturnToTheGathering)
    , ("50025", EncounterSet.ReturnToTheMidnightMasks)
    , ("50032", EncounterSet.ReturnToTheDevourerBelow)
    , ("81001", EncounterSet.CurseOfTheRougarou)
    , ("82001", EncounterSet.CarnevaleOfHorrors)
    ]
