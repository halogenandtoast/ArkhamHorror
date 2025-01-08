{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Scenario (
  module Arkham.Scenario,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
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

fromTarot :: TarotCard -> SourceableWithCardCode
fromTarot t = SourceableWithCardCode (CardCode $ tshow t.arcana) (TarotSource t)

instance HasAbilities TarotCard where
  getAbilities c@(TarotCard facing arcana) = case arcana of
    TheLoversVI ->
      [ restrictedAbility
          (fromTarot c)
          1
          AffectedByTarot
          $ ForcedAbility (Matcher.GameBegins #when)
      ]
    StrengthVIII | facing == Upright -> do
      [ restrictedAbility
          (fromTarot c)
          1
          AffectedByTarot
          $ ForcedAbility (Matcher.GameBegins #when)
        ]
    WheelOfFortuneX -> case facing of
      Upright ->
        [ restrictedAbility (fromTarot c) 1 (AffectedByTarot <> ActExists Matcher.ActCanWheelOfFortuneX)
            $ ReactionAbility (Matcher.RevealChaosToken #when Matcher.You #autofail) Free
        ]
      Reversed ->
        [ restrictedAbility
            (fromTarot c)
            1
            (AffectedByTarot <> AgendaExists Matcher.AgendaCanWheelOfFortuneX)
            $ ForcedAbility (Matcher.RevealChaosToken #when Matcher.You #eldersign)
        ]
    JusticeXI -> case facing of
      Upright ->
        [ groupLimit PerGame
            $ restrictedAbility (fromTarot c) 1 AffectedByTarot
            $ ForcedAbility
              ( Matcher.WouldPlaceDoomCounter
                  #when
                  Matcher.AnySource
                  (Matcher.AgendaTargetMatches Matcher.FinalAgenda)
              )
        ]
      Reversed ->
        [ restrictedAbility (fromTarot c) 1 AffectedByTarot
            $ ForcedAbility (Matcher.AgendaEntersPlay #when Matcher.FinalAgenda)
        ]
    TheDevilXV ->
      [restrictedAbility (fromTarot c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    TheTowerXVI -> do
      -- This is handled by SetupInvestigators below
      [restrictedAbility (fromTarot c) 1 AffectedByTarot $ ForcedAbility Matcher.NotAnyWindow]
    TheStarXVII -> case facing of
      Upright ->
        [ restrictedAbility
            (fromTarot c)
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
            (fromTarot c)
            1
            (AffectedByTarot <> DuringSkillTest Matcher.AnySkillTest)
            $ ForcedAbility (Matcher.RevealChaosToken #when Matcher.You #autofail)
        ]
    TheMoonXVIII -> case facing of
      Upright ->
        [ playerLimit PerGame
            $ restrictedAbility (fromTarot c) 1 AffectedByTarot
            $ ForcedAbility (Matcher.DeckWouldRunOutOfCards #when Matcher.You)
        ]
      Reversed -> [restrictedAbility (fromTarot c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    JudgementXX ->
      [restrictedAbility (fromTarot c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameBegins #when)]
    TheWorldXXI ->
      [restrictedAbility (fromTarot c) 1 AffectedByTarot $ ForcedAbility (Matcher.GameEnds #when)]
    _ -> []

tarotInvestigator :: HasGame m => TarotCard -> m (Maybe InvestigatorId)
tarotInvestigator card = do
  tarotCards <- Map.assocs <$> scenarioField ScenarioTarotCards
  pure $ case find (\(_, vs) -> card `elem` vs) tarotCards of
    Nothing -> Nothing
    Just (InvestigatorTarot iid, _) -> Just iid
    Just (GlobalTarot, _) -> Nothing

instance HasModifiersFor TarotCard where
  getModifiersFor c@(TarotCard facing arcana) = do
    let source = TarotSource c
    case arcana of
      TheFool0 -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        isDefeated <- lift $ iid <=~> Matcher.DefeatedInvestigator
        pure
          $ case facing of
            Upright -> [XPModifier "The Fool 0" 2 | not isDefeated]
            Reversed -> [XPModifier "The Fool 0" (-2) | isDefeated]
      TheMagicianI -> modifySelectMaybeWith source Matcher.Anyone setActiveDuringSetup \iid -> do
        liftGuardM $ affectedByTarot iid c
        firstTurn <- lift $ scenarioFieldMap ScenarioTurn (== 1)
        pure
          $ case facing of
            Upright -> [StartingResources 3]
            Reversed -> StartingResources (-3) : [CannotGainResources | firstTurn]
      TheHighPriestessII -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        history <- lift $ getHistory TurnHistory iid
        currentSkillTypes <- lift getSkillTestSkillTypes
        let
          skillTypes = concatMap fst $ historySkillTestsPerformed history
        guard $ #intellect `notElem` skillTypes && #intellect `elem` currentSkillTypes
        pure
          $ case facing of
            Upright -> [SkillModifier #intellect 1]
            Reversed -> [SkillModifier #intellect (-1)]
      TheEmpressIII -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        history <- lift $ getHistory TurnHistory iid
        currentSkillTypes <- lift getSkillTestSkillTypes
        let
          skillTypes = concatMap fst $ historySkillTestsPerformed history
        guard $ #agility `notElem` skillTypes && #agility `elem` currentSkillTypes
        pure
          $ case facing of
            Upright -> [SkillModifier #agility 1]
            Reversed -> [SkillModifier #agility (-1)]
      TheEmperorIV -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        history <- lift $ getHistory TurnHistory iid
        currentSkillTypes <- lift getSkillTestSkillTypes
        let
          skillTypes = concatMap fst $ historySkillTestsPerformed history
        guard $ #combat `notElem` skillTypes && #combat `elem` currentSkillTypes
        pure
          $ case facing of
            Upright -> [SkillModifier #combat 1]
            Reversed -> [SkillModifier #combat (-1)]
      TheHierophantV -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        history <- lift $ getHistory TurnHistory iid
        currentSkillTypes <- lift getSkillTestSkillTypes
        let
          skillTypes = concatMap fst $ historySkillTestsPerformed history
        guard $ #willpower `notElem` skillTypes && #willpower `elem` currentSkillTypes
        pure
          $ case facing of
            Upright -> [SkillModifier #willpower 1]
            Reversed -> [SkillModifier #willpower (-1)]
      TheLoversVI -> pure mempty
      TheChariotVII -> modifySelectMaybeWith source Matcher.Anyone setActiveDuringSetup \iid -> do
        liftGuardM $ affectedByTarot iid c
        firstTurn <- lift $ scenarioFieldMap ScenarioTurn (== 1)
        pure
          $ case facing of
            Upright -> [StartingHand 2]
            Reversed -> StartingHand (-2) : [CannotDrawCards | firstTurn]
      StrengthVIII ->
        case facing of
          Upright -> pure mempty
          Reversed -> modifySelectMaybe source Matcher.Anyone \iid -> do
            liftGuardM $ affectedByTarot iid c
            liftGuardM $ scenarioFieldMap ScenarioTurn (== 1)
            pure [CannotPlay #asset]
      TheHermitIX -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        pure
          $ case facing of
            Upright -> [HandSize 3]
            Reversed -> [HandSize (-3)]
      WheelOfFortuneX -> pure mempty
      JusticeXI -> pure mempty
      TheHangedManXII -> modifySelectMaybeWith source Matcher.Anyone setActiveDuringSetup \iid -> do
        liftGuardM $ affectedByTarot iid c
        pure
          $ case facing of
            Upright -> [Mulligans 2]
            Reversed -> [CannotMulligan, CannotReplaceWeaknesses]
      DeathXIII -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        pure
          $ case facing of
            Upright -> [HealthModifier 1]
            Reversed -> [HealthModifier (-1)]
      TemperanceXIV -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        pure
          $ case facing of
            Upright -> [SanityModifier 1]
            Reversed -> [SanityModifier (-1)]
      TheDevilXV -> pure mempty
      TheTowerXVI -> pure mempty
      TheStarXVII -> pure mempty
      TheMoonXVIII -> pure mempty
      TheSunXIX -> modifySelectMaybe source Matcher.Anyone \iid -> do
        liftGuardM $ affectedByTarot iid c
        liftGuardM $ scenarioFieldMap ScenarioTurn (== 1)
        pure
          $ case facing of
            Upright -> [AdditionalActions "THE SUN · XIX" source 2]
            Reversed -> [FewerActions 2]
      JudgementXX -> pure mempty
      TheWorldXXI -> pure mempty

instance HasModifiersFor Scenario where
  getModifiersFor (Scenario a) = do
    getModifiersFor a
    traverse_ getModifiersFor (concat . toList $ attr scenarioTarotCards a)

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
        results <- select (Matcher.InHandOf (Matcher.InvestigatorWithId investigator) <> #asset)
        resources <- getSpendableResources investigator
        cards <-
          filterM
            ( getIsPlayableWithResources
                investigator
                source
                (resources + 2)
                (UnpaidCost NoAction)
                [duringTurnWindow investigator]
            )
            results
        player <- getPlayer investigator
        choices <- for cards \c -> do
          enabled <- costModifier source investigator (ReduceCostOf (Matcher.CardWithId $ toCardId c) 2)
          pure $ targetLabel c [enabled, PayCardCost investigator c [duringTurnWindow investigator]]

        pure $ Just $ chooseOne player $ Label "Do not play asset" [] : choices
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
          player <- getPlayer investigator

          agendas <- select Matcher.AnyAgenda
          push
            $ chooseOrRunOne
              player
              [targetLabel agenda [PlaceDoom source (toTarget agenda) 1] | agenda <- agendas]
      -- cancelDoom 1
      pure x
    UseThisAbility _ source@(TarotSource card@(TarotCard facing TheDevilXV)) 1 -> do
      investigatorPlayers <- filterM ((`affectedByTarot` card) . fst) =<< getInvestigatorPlayers
      case facing of
        Upright -> do
          pushAll
            [ chooseOne
              player
              [ Label ("Add " <> slotName slotType <> " Slot") [AddSlot investigator slotType (Slot source [])]
              | slotType <- allSlotTypes
              ]
            | (investigator, player) <- investigatorPlayers
            ]
        Reversed -> do
          for_ investigatorPlayers $ \(investigator, player) -> do
            slotTypes <- keys . filterMap notNull <$> field Field.InvestigatorSlots investigator
            pushWhen (notNull slotTypes)
              $ chooseN
                player
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
              (Matcher.basic $ Matcher.CardWithSubType BasicWeakness)
              (RemoveFoundFromGame iid 1)
            | iid <- investigators
            ]
        Reversed ->
          for_ investigators $ \investigator ->
            push
              $ SearchCollectionForRandom investigator source
              $ Matcher.BasicWeaknessCard
      pure x
    RequestedPlayerCard iid (TarotSource (TarotCard Reversed TheTowerXVI)) (Just c) _ -> do
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [toCard c]
      pure x
    UseThisAbility iid source@(TarotSource (TarotCard facing TheStarXVII)) 1 -> do
      player <- getPlayer iid
      case facing of
        Upright -> do
          canHealDamage <- Helpers.canHaveDamageHealed source iid
          canHealHorror <- Helpers.canHaveHorrorHealed source iid
          push
            $ chooseOrRunOne player
            $ [DamageLabel iid [HealDamage (toTarget iid) source 1] | canHealDamage]
            <> [HorrorLabel iid [HealHorror (toTarget iid) source 1] | canHealHorror]
        Reversed -> do
          push
            $ chooseOne
              player
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
          getDraw [] = Nothing
          getDraw (Would batchId' msgs : _) | batchId' == batchId = getDraw msgs
          getDraw (Do (EmptyDeck _ (Just draw)) : _) = Just draw
          getDraw (_ : rest) = getDraw rest
        mDrawing <- fromQueue getDraw
        cards <- map toCard . take 10 . reverse <$> field Field.InvestigatorDiscard iid
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ Label
                "Shuffle the bottom 10 cards of your discard back into your Deck"
                $ [IgnoreBatch batchId, ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) cards]
                <> maybeToList mDrawing
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
      unless (null weaknesses)
        $ push
        $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) (map toCard weaknesses)
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
                player <- getPlayer iid
                if (hasPhysicalTrauma || hasMentalTrauma)
                  then pure $ Just (iid, player, hasPhysicalTrauma, hasMentalTrauma)
                  else pure Nothing

          pushAll
            $ [ chooseOne player
                $ [Label "Remove a physical trauma" [HealTrauma iid 1 0] | hasPhysical]
                <> [Label "Remove a mental trauma" [HealTrauma iid 0 1] | hasMental]
                <> [Label "Do not remove trauma" []]
              | (iid, player, hasPhysical, hasMental) <- investigatorsWhoCanHealTrauma
              ]
        Reversed -> do
          defeatedInvestigators <-
            filterM (`affectedByTarot` card) =<< select Matcher.DefeatedInvestigator
          defeatedInvestigatorPlayers <- traverse (traverseToSnd getPlayer) defeatedInvestigators
          pushAll
            $ [ chooseOne
                player
                [ Label "Suffer physical trauma" [SufferTrauma iid 1 0]
                , Label "Suffer mental trauma" [SufferTrauma iid 0 1]
                ]
              | (iid, player) <- defeatedInvestigatorPlayers
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
    ScenarioResolution _ -> overAttrs (\a -> a & inResolutionL .~ True) <$> go
    SetupInvestigators -> do
      result <- go
      let isTowerXVI = (== TheTowerXVI) . toTarotArcana
      for_ (filter isTowerXVI $ concat $ toList $ attr scenarioTarotCards s) $ \card -> do
        lead <- getLead
        mInvestigator <- tarotInvestigator card
        let investigator = fromMaybe lead mInvestigator
        let abilities = getAbilities card
        player <- getPlayer investigator
        for_ abilities $ \ability -> do
          push $ chooseOne player [AbilityLabel investigator ability [] [] []]
      pure result
    PreScenarioSetup -> do
      result <- go
      observed <- select $ Matcher.DeckWith $ Matcher.HasCard $ Matcher.cardIs Assets.observed4
      for_ observed $ \iid -> do
        push $ DrawAndChooseTarot iid Upright 3
      damned <- select $ Matcher.DeckWith $ Matcher.HasCard $ Matcher.cardIs Treacheries.damned
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
      else do
        case chaosTokenFace of
          CurseToken -> pure $ ChaosTokenValue chaosTokenFace (NegativeModifier 2)
          BlessToken -> pure $ ChaosTokenValue chaosTokenFace (PositiveModifier 2)
          FrostToken -> do
            revealed <- map (.face) <$> getSkillTestRevealedChaosTokens
            pure
              $ ChaosTokenValue chaosTokenFace
              $ if count (== #frost) revealed == 2 then AutoFailModifier else NegativeModifier 1
          _ -> getChaosTokenValue iid chaosTokenFace s

lookupScenario :: ScenarioId -> Difficulty -> Scenario
lookupScenario scenarioId =
  case lookup (unScenarioId scenarioId) allScenarios of
    Nothing -> error $ "Unknown scenario: " <> show scenarioId
    Just (SomeScenario f) -> Scenario . f

data SomeScenario = forall a. IsScenario a => SomeScenario (Difficulty -> a)

scenarioCard :: CardCode -> Name -> EncounterSet -> CardDef
scenarioCard cCode name ecSet =
  (emptyCardDef cCode name ScenarioType)
    { cdEncounterSet = Just ecSet
    , cdEncounterSetQuantity = Just 1
    , cdDoubleSided = True
    , cdLevel = Nothing
    }

allScenarioCards :: Map CardCode CardDef
allScenarioCards =
  mapFromList $ flip map (filter ((`notElem` duplicatedScenarios) . fst) $ mapToList allScenarios) $ \(c, SomeScenario s) -> do
    let ecSet = fromJustNote "you forgot to add the encounter set" $ lookup c scenarioEncounterSets
        name = scenarioName $ toAttrs $ Scenario (s Easy)
        normalizeCardCode = \case
          "08501a" -> "08501"
          other -> other
    (normalizeCardCode c, scenarioCard (normalizeCardCode c) name ecSet)

duplicatedScenarios :: [CardCode]
duplicatedScenarios = ["08501c"]

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
    , ("06039", SomeScenario beyondTheGatesOfSleep)
    , ("06063", SomeScenario wakingNightmare)
    , ("06119", SomeScenario theSearchForKadath)
    , ("06168", SomeScenario aThousandShapesOfHorror)
    , ("06206", SomeScenario darkSideOfTheMoon)
    , ("06247", SomeScenario pointOfNoReturn)
    , ("06286", SomeScenario whereTheGodsDwell)
    , ("06333", SomeScenario weaverOfTheCosmos)
    , ("07041", SomeScenario thePitOfDespair)
    , ("07056", SomeScenario theVanishingOfElinaHarper)
    , ("07123", SomeScenario inTooDeep)
    , ("07163", SomeScenario devilReef)
    , ("07198", SomeScenario horrorInHighGear)
    , ("07231", SomeScenario aLightInTheFog)
    , ("07274", SomeScenario theLairOfDagon)
    , ("07311", SomeScenario intoTheMaelstrom)
    , ("08501a", SomeScenario iceAndDeathPart1)
    , ("08501b", SomeScenario iceAndDeathPart2)
    , ("08501c", SomeScenario iceAndDeathPart3)
    , ("08549", SomeScenario fatalMirage)
    , ("08596", SomeScenario toTheForbiddenPeaks)
    , ("50011", SomeScenario returnToTheGathering)
    , ("50025", SomeScenario returnToTheMidnightMasks)
    , ("50032", SomeScenario returnToTheDevourerBelow)
    , ("81001", SomeScenario curseOfTheRougarou)
    , ("82001", SomeScenario carnevaleOfHorrors)
    , ("84001", SomeScenario murderAtTheExcelsiorHotel)
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
    , ("06039", EncounterSet.BeyondTheGatesOfSleep)
    , ("06063", EncounterSet.WakingNightmare)
    , ("06119", EncounterSet.TheSearchForKadath)
    , ("06168", EncounterSet.AThousandShapesOfHorror)
    , ("06206", EncounterSet.DarkSideOfTheMoon)
    , ("06247", EncounterSet.PointOfNoReturn)
    , ("06286", EncounterSet.WhereTheGodsDwell)
    , ("06333", EncounterSet.WeaverOfTheCosmos)
    , ("07041", EncounterSet.ThePitOfDespair)
    , ("07056", EncounterSet.TheVanishingOfElinaHarper)
    , ("07123", EncounterSet.InTooDeep)
    , ("07163", EncounterSet.DevilReef)
    , ("07198", EncounterSet.HorrorInHighGear)
    , ("07231", EncounterSet.ALightInTheFog)
    , ("07274", EncounterSet.TheLairOfDagon)
    , ("07311", EncounterSet.IntoTheMaelstrom)
    , ("08501a", EncounterSet.IceAndDeath)
    , ("08501b", EncounterSet.IceAndDeath)
    , ("08501c", EncounterSet.IceAndDeath)
    , ("08549", EncounterSet.FatalMirage)
    , ("08596", EncounterSet.ToTheForbiddenPeaks)
    , ("50011", EncounterSet.ReturnToTheGathering)
    , ("50025", EncounterSet.ReturnToTheMidnightMasks)
    , ("50032", EncounterSet.ReturnToTheDevourerBelow)
    , ("81001", EncounterSet.CurseOfTheRougarou)
    , ("82001", EncounterSet.CarnevaleOfHorrors)
    , ("84001", EncounterSet.MurderAtTheExcelsiorHotel)
    ]
