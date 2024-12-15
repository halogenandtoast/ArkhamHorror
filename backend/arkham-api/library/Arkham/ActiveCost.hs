{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.ActiveCost (module Arkham.ActiveCost, module X) where

import Arkham.ActiveCost.Base as X

import Arkham.Ability hiding (PaidCost)
import Arkham.Action hiding (TakenAction)
import Arkham.Action qualified as Action
import Arkham.Asset.Types (
  Field (
    AssetCard,
    AssetCardsUnderneath,
    AssetController,
    AssetName,
    AssetSealedChaosTokens,
    AssetUses
  ),
 )
import Arkham.Attack
import Arkham.Campaigns.EdgeOfTheEarth.Helpers (addTekelili)
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Constants
import Arkham.Cost.FieldCost
import Arkham.Deck qualified as Deck
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Types (Field (EnemySealedChaosTokens))
import Arkham.Exception
import {-# SOURCE #-} Arkham.Game (withoutCanModifiers)
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Calculation
import Arkham.Helpers.Card (extendedCardMatch)
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Customization
import Arkham.Helpers.Effect (createCardEffect)
import Arkham.Helpers.Message
import Arkham.Helpers.SkillTest (beginSkillTest, getSkillTestDifficulty, getSkillTestTarget)
import Arkham.Id
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (
  AssetCard,
  EventCard,
  LocationCard,
  PlayCard,
  SkillCard,
 )
import Arkham.Name
import Arkham.Prelude
import Arkham.Projection
import Arkham.Scenario.Deck (ScenarioDeckKey (TekeliliDeck))
import Arkham.Scenario.Types (Field (..))
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Token qualified as Token
import Arkham.Window (Window (..), mkAfter, mkWhen)
import Arkham.Window qualified as Window
import Control.Lens (non, transform)
import GHC.Records

activeCostActions :: ActiveCost -> [Action]
activeCostActions ac = case ac.target of
  ForAbility a -> #activate : a.actions
  ForCard isPlayAction c -> [#play | isPlayAction == IsPlayAction] <> c.actions
  ForCost _ -> []
  ForAdditionalCost _ -> []

instance HasField "actions" ActiveCost [Action] where
  getField = activeCostActions

addActiveCostCost :: Cost -> ActiveCost -> ActiveCost
addActiveCostCost cost ac = ac & costsL <>~ cost

activeCostSource :: ActiveCost -> Source
activeCostSource ac = case activeCostTarget ac of
  ForAbility a -> toSource a
  ForCard _ c -> CardIdSource c.id
  ForCost c -> CardIdSource c.id
  ForAdditionalCost c -> BatchSource c

instance HasField "source" ActiveCost Source where
  getField = activeCostSource

costsL :: Lens' ActiveCost Cost
costsL = lens activeCostCosts $ \m x -> m {activeCostCosts = x}

costPaymentsL :: Lens' ActiveCost Payment
costPaymentsL = lens activeCostPayments $ \m x -> m {activeCostPayments = x}

costSealedChaosTokensL :: Lens' ActiveCost [ChaosToken]
costSealedChaosTokensL = lens activeCostSealedChaosTokens $ \m x -> m {activeCostSealedChaosTokens = x}

getActionCostModifier :: HasGame m => ActiveCost -> m Int
getActionCostModifier ac = do
  let iid = ac.investigator
  takenActions <- field InvestigatorActionsTaken iid
  performedActions <- field InvestigatorActionsPerformed iid
  modifiers <- getModifiers iid
  pure $ foldr (applyModifier takenActions performedActions) 0 modifiers
 where
  actions = case ac.actions of
    [] -> error "expected action"
    as -> as
  applyModifier takenActions performedActions (AdditionalActionCostOf match m) n =
    -- For cards we've already calculated the cost as an additional cost for
    -- the action specifically
    case ac.target of
      ForCard {} -> n
      _ -> if any (matchTarget takenActions performedActions match) actions then n + m else n
  applyModifier _ _ _ n = n

countAdditionalActionPayments :: Payment -> Int
countAdditionalActionPayments AdditionalActionPayment = 1
countAdditionalActionPayments (Payments ps) =
  sum $ map countAdditionalActionPayments ps
countAdditionalActionPayments _ = 0

startAbilityPayment
  :: (HasQueue Message m, HasGame m)
  => ActiveCost
  -> InvestigatorId
  -> Window
  -> AbilityType
  -> Source
  -> Bool
  -> m ()
startAbilityPayment activeCost@ActiveCost {activeCostId} iid window abilityType source provokeAttacksOfOpportunity =
  case abilityType of
    Objective aType -> startAbilityPayment activeCost iid window aType source provokeAttacksOfOpportunity
    DelayedAbility aType -> startAbilityPayment activeCost iid window aType source provokeAttacksOfOpportunity
    ForcedAbility _ -> pure ()
    SilentForcedAbility _ -> pure ()
    Haunted -> pure ()
    ServitorAbility _ -> pure ()
    Cosmos -> pure ()
    ForcedAbilityWithCost {} -> push (PayCosts activeCostId)
    AbilityEffect {} -> push (PayCosts activeCostId)
    FastAbility' _ mAction ->
      pushAll $ PayCosts activeCostId : [PerformedActions iid [action] | action <- toList mAction]
    ForcedWhen _ aType -> startAbilityPayment activeCost iid window aType source provokeAttacksOfOpportunity
    CustomizationReaction {} -> push (PayCosts activeCostId)
    ConstantReaction {} -> push (PayCosts activeCostId)
    ReactionAbility {} -> push (PayCosts activeCostId)
    ActionAbilityWithSkill actions' _ _ -> handleActions $ Action.Activate : actions'
    ActionAbility actions' _ -> handleActions $ Action.Activate : actions'
 where
  checkAttackOfOpportunity actions =
    not provokeAttacksOfOpportunity && all (`notElem` nonAttackOfOpportunityActions) actions
  handleActions actions = do
    beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid action | action <- actions]
    pushAll
      $ [BeginAction, beforeWindowMsg, PayCosts activeCostId, TakenActions iid actions]
      <> [CheckAttackOfOpportunity iid False | checkAttackOfOpportunity actions]

nonAttackOfOpportunityActions :: [Action]
nonAttackOfOpportunityActions = [#fight, #evade, #resign, #parley]

payCost
  :: forall m
   . (HasGame m, HasQueue Message m, HasCallStack, CardGen m)
  => Message
  -> ActiveCost
  -> InvestigatorId
  -> Bool
  -> Cost
  -> m ActiveCost
payCost msg c iid skipAdditionalCosts cost = do
  let acId = c.id
  let withPayment payment = pure $ c & costPaymentsL <>~ payment
  let source = c.source
  let actions = c.actions
  let pay = PayCost acId iid skipAdditionalCosts
  player <- getPlayer iid
  case cost of
    ShuffleTopOfScenarioDeckIntoYourDeck n TekeliliDeck -> do
      runQueueT $ addTekelili iid . take n =<< getScenarioDeck TekeliliDeck
      pure c
    ShuffleTopOfScenarioDeckIntoYourDeck n deckKey -> do
      push . shuffleCardsIntoDeck iid . take n =<< getScenarioDeck deckKey
      pure c
    GroupSpendKeyCost key _ -> do
      push $ PlaceKey ScenarioTarget key
      pure c
    SpendKeyCost key -> do
      push $ PlaceKey ScenarioTarget key
      pure c
    PlaceKeyCost target key -> do
      push $ PlaceKey target key
      pure c
    CostToEnterUnrevealed _ -> do
      -- we don't pay this directly, it must be unwrapped elsewhere
      pure c
    ChooseExtendedCardCost mtch -> do
      cards <- select mtch
      push $ chooseOne player $ targetLabels cards $ only . pay . ChosenCardCost . toCardId
      pure c
    ChosenCardCost cid -> withPayment $ ChosenCardPayment cid
    ChooseEnemyCost mtch -> do
      enemies <- select mtch
      push $ chooseOne player $ targetLabels enemies $ only . pay . ChosenEnemyCost
      pure c
    ChosenEnemyCost eid -> withPayment $ ChosenEnemyPayment eid
    CostIfCustomization customization cost1 cost2 -> do
      case source of
        (CardIdSource cid) -> do
          getCard cid >>= \case
            PlayerCard pc ->
              payCost msg c iid skipAdditionalCosts
                $ if pc `hasCustomization` customization then cost1 else cost2
            _ -> error "Not implemented"
        _ -> error "Not implemented"
    CostIfEnemy mtchr cost1 cost2 -> do
      hasEnemy <- selectAny mtchr
      payCost msg c iid skipAdditionalCosts $ if hasEnemy then cost1 else cost2
    CostWhenEnemy mtchr cost' -> do
      hasEnemy <- selectAny mtchr
      if hasEnemy
        then payCost msg c iid skipAdditionalCosts cost'
        else pure c
    ArchiveOfConduitsUnidentifiedCost -> do
      locations <- select Anywhere
      push
        $ chooseOrRunN
          player
          4
          [ targetLabel location [PlaceTokens source (toTarget location) Token.Leyline 1]
          | location <- locations
          ]
      pure c
    NonBlankedCost cost' -> do
      mods <- getModifiers (sourceToTarget source)
      if Blank `elem` mods
        then pure c
        else payCost msg c iid skipAdditionalCosts cost'
    GloriaCost -> do
      mtarget <- getSkillTestTarget
      case mtarget of
        Nothing -> error "not in skill test"
        Just t -> do
          gloria <- selectJust $ investigatorIs Investigators.gloriaGoldberg
          cardsUnderneath <- field InvestigatorCardsUnderneath gloria
          traits <- targetTraits t
          let cards = filter (\card -> any (\trait -> card `cardMatch` CardWithTrait trait) traits) cardsUnderneath
          pushAll
            [ FocusCards cards
            , chooseOne
                player
                [ targetLabel (CardIdTarget $ toCardId card) [ObtainCard card.id, AddToEncounterDiscard ec]
                | card@(EncounterCard ec) <- cards
                ]
            ]
      pure c
    EnemyAttackCost eid -> do
      push $ toMessage $ enemyAttack eid source iid
      pure c
    DrawEncounterCardsCost n -> do
      pushAll $ replicate n $ drawEncounterCard iid source
      pure c
    SkillTestCost stsource sType n -> do
      sid <- getRandom
      push $ beginSkillTest sid iid stsource (sourceToTarget stsource) sType n
      pure c
    AsIfAtLocationCost _ _ -> error "Can not be paid because withModifiers only HasGame, but can't adjust the queue"
    ShuffleAttachedCardIntoDeckCost target cardMatcher -> do
      case target of
        AssetTarget aid -> do
          cards <- fieldMap AssetCardsUnderneath (filter (`cardMatch` cardMatcher)) aid
          case cards of
            [] -> error "no cards underneath"
            (x : _) -> push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [x]
        _ -> error "Unhandle target type"
      pure c
    ShuffleIntoDeckCost target -> do
      push $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) target
      pure c
    ShuffleBondedCost n cCode -> do
      bondedCards <- fieldMap InvestigatorBondedCards (take n . filter ((== cCode) . toCardCode)) iid
      push $ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) bondedCards
      pure c
    ResolveEachHauntedAbility lid -> do
      hauntedAbilities <- select $ HauntedAbility <> AbilityOnLocation (LocationWithId lid)
      pushIfAny hauntedAbilities
        $ chooseOneAtATime player [AbilityLabel iid ab [] [] [] | ab <- hauntedAbilities]
      -- No need to record payment... yet
      pure c
    OrCost xs -> do
      xs' <- filterM (getCanAffordCost iid source actions c.windows) xs
      push
        $ chooseOne player
        $ map (\x -> Label (displayCostType x) [pay x]) xs'
      pure c
    OptionalCost x -> do
      canAfford <- getCanAffordCost iid source actions [] x
      pushWhen canAfford $ chooseOne player [Label (displayCostType x) [pay x], Label "Do not pay" []]
      pure c
    Costs xs -> do
      pushAll $ map pay xs
      pure c
    UpTo calc cost' -> do
      n <- calculate calc
      if n == 0
        then pure c
        else do
          canAfford <- andM $ map (\a -> getCanAffordCost iid source [a] [] cost') actions
          maxUpTo <- case cost' of
            ResourceCost resources -> do
              availableResources <- getSpendableResources iid
              pure $ min n (availableResources `div` resources)
            SealCost matcher -> selectCount matcher
            _ -> pure n
          name <- fieldMap InvestigatorName toTitle iid
          choiceId <- getRandom
          pushWhen canAfford
            $ Ask player
            $ ChoosePaymentAmounts
              ("Pay " <> displayCostType cost)
              Nothing
              [PaymentAmountChoice choiceId iid 0 maxUpTo name $ pay cost']
          pure c
    DiscardTopOfDeckCost n -> do
      cards <- fieldMap InvestigatorDeck (map PlayerCard . take n . unDeck) iid
      push $ DiscardTopOfDeck iid n source Nothing
      withPayment $ DiscardCardPayment cards
    IncreaseCostOfThis cardId n -> do
      ems <- effectModifiers source [IncreaseCostOf (CardWithId cardId) n]
      push $ CreateWindowModifierEffect (EffectCardCostWindow cardId) ems source (toTarget cardId)
      pure c
    ExhaustCost target -> do
      push $ Exhaust target
      withPayment $ ExhaustPayment [target]
    ExhaustAssetCost matcher -> do
      assets <- select $ matcher <> AssetReady
      push $ chooseOne player $ targetLabels assets $ only . pay . exhaust
      pure c
    ExhaustXAssetCost matcher -> do
      assets <- select $ matcher <> AssetReady
      push
        $ chooseSome1 player "Done exhausting"
        $ targetLabels assets
        $ only
        . pay
        . exhaust
      pure c
    SealCost matcher -> do
      targets <-
        filterM (\t -> matchChaosToken iid t matcher)
          =<< scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
      pushAll
        [ FocusChaosTokens targets
        , chooseOne player $ targetLabels targets $ only . pay . SealChaosTokenCost
        , UnfocusChaosTokens
        ]
      pure c
    SealMultiCost n matcher -> do
      pushAll $ replicate n $ pay (SealCost matcher)
      pure c
    SealChaosTokenCost token -> do
      push $ SealChaosToken token
      pure $ c & costPaymentsL <>~ SealChaosTokenPayment token & costSealedChaosTokensL %~ (token :)
    ReleaseChaosTokensCost n matcher -> do
      case matcher of
        SealedOnAsset assetMatcher tokenMatcher' -> do
          mAsset <- selectOne assetMatcher
          case mAsset of
            Nothing -> error "Unhandled asset"
            Just aid -> do
              tokens <- filterM (<=~> IncludeSealed tokenMatcher') =<< field AssetSealedChaosTokens aid
              pushAll
                [ FocusChaosTokens tokens
                , chooseN player n $ targetLabels tokens $ only . pay . ReleaseChaosTokenCost
                , UnfocusChaosTokens
                ]
        SealedOnEnemy enemyMatcher tokenMatcher' -> do
          mEnemy <- selectOne enemyMatcher
          case mEnemy of
            Nothing -> error "Unhandled enemy"
            Just aid -> do
              tokens <- filterM (<=~> IncludeSealed tokenMatcher') =<< field EnemySealedChaosTokens aid
              pushAll
                [ FocusChaosTokens tokens
                , chooseN player n $ targetLabels tokens $ only . pay . ReleaseChaosTokenCost
                , UnfocusChaosTokens
                ]
        _ -> do
          let
            handleSource = \case
              AbilitySource t _ -> handleSource t
              AssetSource aid -> do
                tokens <- filterM (<=~> IncludeSealed matcher) =<< field AssetSealedChaosTokens aid
                pushAll
                  [ FocusChaosTokens tokens
                  , chooseN player n $ targetLabels tokens $ only . pay . ReleaseChaosTokenCost
                  , UnfocusChaosTokens
                  ]
              _ -> error "Unhandled source for releasing tokens cost"
          handleSource source
      pure c
    ReleaseChaosTokenCost t -> do
      push $ UnsealChaosToken t
      withPayment $ ReleaseChaosTokenPayment t
    ReturnChaosTokensToPoolCost n matcher -> do
      tokens <- select matcher
      pushAll
        [ FocusChaosTokens tokens
        , chooseN player n $ targetLabels tokens $ only . pay . ReturnChaosTokenToPoolCost
        , UnfocusChaosTokens
        ]
      pure c
    ReturnChaosTokenToPoolCost t -> do
      push $ ReturnChaosTokensToPool [t]
      withPayment $ ReturnChaosTokenToPoolPayment t
    SupplyCost matcher supply -> do
      iid' <- selectJust $ InvestigatorWithSupply supply <> InvestigatorAt matcher
      push $ UseSupply iid' supply
      withPayment $ SupplyPayment supply
    DiscardCost zone target -> do
      card <- targetToCard target
      pushAll [DiscardedCost target, toDiscardBy iid c.source target]
      withPayment $ DiscardPayment [(zone, card)]
    DiscardAssetCost matcher -> do
      assets <- select (matcher <> DiscardableAsset)
      push $ chooseOne player $ targetLabels assets $ only . pay . discardCost
      pure c
    DiscardRandomCardCost -> do
      push $ toMessage $ randomDiscard iid c.source
      pure c
    DiscardCardCost card -> do
      push $ toMessage $ discardCard iid c.source card
      withPayment $ DiscardCardPayment [card]
    DiscardUnderneathCardCost assetId cardMatcher -> do
      cards <- filterM (<=~> cardMatcher) =<< field AssetCardsUnderneath assetId
      let
        discardIt = \case
          PlayerCard pc -> [AddToDiscard owner pc | owner <- maybeToList (pcOwner pc)]
          EncounterCard ec -> [AddToEncounterDiscard ec]
          _ -> error "Unhandled"
      pushAll
        [ FocusCards cards
        , chooseOrRunOne player [targetLabel card (UnfocusCards : discardIt card) | card <- cards]
        ]
      pure c
    DiscardDrawnCardCost -> do
      let
        getDrawnCard [] = error "can not find drawn card in windows"
        getDrawnCard (x : xs) = case x of
          (windowType -> Window.DrawCard _ card' _) -> card'
          _ -> getDrawnCard xs
        card = getDrawnCard c.windows
      push $ toMessage $ discardCard iid c.source card
      withPayment $ DiscardCardPayment [card]
    ExileCost target -> do
      push (Exile target)
      withPayment $ ExilePayment [target]
    RemoveCost target -> do
      push (RemoveFromGame target)
      withPayment $ RemovePayment [target]
    RevealCost cardId -> do
      push $ RevealCard cardId
      pure c
    EnemyDoomCost x matcher -> do
      enemies <- select matcher
      push $ chooseOrRunOne player [targetLabel enemy [placeDoom source enemy x] | enemy <- enemies]
      withPayment $ DoomPayment x
    RemoveEnemyDamageCost x matcher -> do
      n <- getGameValue x
      enemies <- select $ matcher <> EnemyWithDamage (atLeast n)
      push
        $ chooseOrRunOne
          player
          [targetLabel enemy [RemoveTokens source (toTarget enemy) #damage n] | enemy <- enemies]
      pure c
    DoomCost _ (AgendaMatcherTarget matcher) x -> do
      agendas <- selectMap AgendaTarget matcher
      pushAll [PlaceDoom source target x | target <- agendas]
      withPayment $ DoomPayment (x * length agendas)
    DoomCost _ target x -> do
      push $ PlaceDoom source target x
      withPayment $ DoomPayment x
    HorrorCost _ target x -> case target of
      InvestigatorTarget iid' | iid' == iid -> do
        push $ assignHorror iid source x
        withPayment $ HorrorPayment x
      YouTarget -> do
        push $ assignHorror iid source x
        withPayment $ HorrorPayment x
      AssetTarget aid -> do
        push $ DealAssetDamage aid source 0 x
        withPayment $ HorrorPayment x
      _ -> error "can't target for horror cost"
    HorrorCostX source' -> do
      -- see: The Black Book as that is the only card that uses this
      let
        getPlayedCard [] = error "can not find played card in windows"
        getPlayedCard (x : xs) = case x of
          (windowType -> Window.PlayCard _ card') -> card'
          _ -> getPlayedCard xs
        cardPlay = getPlayedCard c.windows
      availableResources <- getSpendableResources iid
      requiredResources <- getModifiedCardCost iid cardPlay.card
      let minimumHorror = max 1 (requiredResources - availableResources)
      sanity <- field InvestigatorRemainingSanity iid
      name <- fieldMap InvestigatorName toTitle iid
      choiceId <- getRandom
      push
        $ Ask player
        $ ChoosePaymentAmounts
          "Pay X Horror"
          Nothing
          [ PaymentAmountChoice choiceId iid minimumHorror sanity name
              $ pay (HorrorCost source' (InvestigatorTarget iid) 1)
          ]
      pure c
    DamageCost _ target x -> case target of
      InvestigatorTarget iid' | iid' == iid -> do
        push $ assignDamage iid source x
        withPayment $ DamagePayment x
      YouTarget -> do
        push $ assignDamage iid source x
        withPayment $ DamagePayment x
      AssetTarget aid -> do
        push $ DealAssetDamage aid source x 0
        withPayment $ DamagePayment x
      _ -> error "can't target for damage cost"
    DirectDamageCost _ investigatorMatcher x -> do
      investigators <- select investigatorMatcher
      case investigators of
        [iid'] -> do
          push $ InvestigatorDirectDamage iid' source x 0
          withPayment $ DirectDamagePayment x
        _ -> error "exactly one investigator expected for direct damage"
    InvestigatorDamageCost source' investigatorMatcher damageStrategy x -> do
      investigators <- select investigatorMatcher
      push
        $ chooseOrRunOne
          player
          [ targetLabel iid' [InvestigatorAssignDamage iid' source' damageStrategy x 0]
          | iid' <- investigators
          ]
      withPayment $ InvestigatorDamagePayment x
    FieldResourceCost (FieldCost mtchr fld) -> do
      ns <- nub <$> selectFields fld mtchr
      case ns of
        [] -> pure ()
        [n] -> push $ PayCost acId iid True (ResourceCost n)
        _ -> do
          resources <- getSpendableResources iid
          name <- fieldMap InvestigatorName toTitle iid
          choiceId <- getRandom
          push
            $ Ask player
            $ ChoosePaymentAmounts
              "Pay X resources"
              (Just $ AmountOneOf ns)
              [PaymentAmountChoice choiceId iid 0 resources name (PayCost acId iid True (ResourceCost 1))]
      pure c
    MaybeFieldResourceCost (MaybeFieldCost mtchr fld) -> do
      ns <- nub . catMaybes <$> selectFields fld mtchr
      case ns of
        [] -> pure ()
        [n] -> push $ PayCost acId iid True (ResourceCost n)
        _ -> do
          resources <- getSpendableResources iid
          name <- fieldMap InvestigatorName toTitle iid
          choiceId <- getRandom
          push
            $ Ask player
            $ ChoosePaymentAmounts
              "Pay X resources"
              (Just $ AmountOneOf ns)
              [PaymentAmountChoice choiceId iid 0 resources name (PayCost acId iid True (ResourceCost 1))]
      pure c
    CalculatedResourceCost calc -> do
      n <- calculate calc
      push $ PayCost acId iid True (ResourceCost n)
      pure c
    ScenarioResourceCost n -> do
      push $ RemoveTokens c.source ScenarioTarget Token.Resource n
      withPayment $ ResourcePayment n
    AddFrostTokenCost n -> do
      x <- min n <$> getRemainingFrostTokens
      if x < n
        then error "Not enough frost tokens"
        else do
          batchId <- getRandom
          would <-
            checkWindows
              [ (mkWhen $ Window.WouldAddChaosTokensToChaosBag (Just iid) $ replicate x FrostToken)
                  { windowBatchId = Just batchId
                  }
              ]
          push $ Would batchId $ would : replicate x (AddChaosToken FrostToken)
          withPayment $ AddFrostTokenPayment x
    AddCurseTokenCost n -> do
      x <- min n <$> getRemainingCurseTokens
      if x < n
        then do
          -- we need to parallel rex
          canParallelRex <-
            iid
              <=~> ( InvestigatorIs "90078"
                      <> InvestigatorAt Anywhere
                      <> InvestigatorWithAnyClues
                   )
          if canParallelRex
            then do
              batchId <- getRandom
              push
                $ Would batchId
                $ chooseOne
                  player
                  [ AbilityLabel
                      iid
                      (mkAbility (SourceableWithCardCode @CardCode "90078" iid) 1 $ freeReaction NotAnyWindow)
                      [Window #when (Window.WouldAddChaosTokensToChaosBag (Just iid) $ replicate n #curse) (Just batchId)]
                      []
                      []
                  ]
                : replicate n (AddChaosToken #curse)
              withPayment $ AddCurseTokenPayment x
            else error "should not be here"
        else do
          batchId <- getRandom
          would <-
            checkWindows
              [ (mkWhen $ Window.WouldAddChaosTokensToChaosBag (Just iid) $ replicate x CurseToken)
                  { windowBatchId = Just batchId
                  }
              ]
          push $ Would batchId $ would : replicate x (AddChaosToken CurseToken)
          withPayment $ AddCurseTokenPayment x
    AddCurseTokensCost n m -> do
      canParallelRex <-
        iid <=~> (InvestigatorIs "90078" <> InvestigatorAt Anywhere <> InvestigatorWithAnyClues)
      rex <- if canParallelRex then fieldMap InvestigatorClues (* 2) iid else pure 0

      maxTokens <- min m . (+ rex) <$> getRemainingCurseTokens

      push
        $ Ask player
        $ QuestionLabel ("Pay " <> displayCostType cost) Nothing
        $ DropDown
          [ (tshow x, pay (AddCurseTokenCost x))
          | x <- [n .. maxTokens]
          ]
      pure c
    AddCurseTokensEqualToShroudCost -> do
      mloc <- field InvestigatorLocation iid
      shroud <- maybe (pure 0) (fieldJust LocationShroud) mloc
      x <- min shroud <$> getRemainingCurseTokens
      pushAll $ replicate x $ AddChaosToken CurseToken
      pure c
    AddCurseTokensEqualToSkillTestDifficulty -> do
      getSkillTestDifficulty >>= \case
        Nothing -> error "Not valid"
        Just difficulty -> do
          x <- min difficulty <$> getRemainingCurseTokens
          pushAll $ replicate x $ AddChaosToken CurseToken
          pure c
    ResourceCost x -> do
      case activeCostTarget c of
        ForAbility {} -> push $ SpendResources iid x
        ForCost {} -> push $ SpendResources iid x
        ForAdditionalCost {} -> push $ SpendResources iid x
        ForCard _ card -> do
          iids <- getInvestigators
          iidsWithModifiers <- for iids $ \iid' -> do
            modifiers <- getModifiers (InvestigatorTarget iid')
            pure (iid', modifiers)
          canHelpPay <- flip filterM iidsWithModifiers $ \(iid', modifiers) ->
            do
              flip anyM modifiers $ \case
                CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher ->
                  andM
                    [ elem iid <$> select iMatcher
                    , pure $ cardMatch card cMatcher
                    , pure $ iid /= iid'
                    ]
                _ -> pure False

          resourcesFromAssets <-
            concatForM iidsWithModifiers \(iid', modifiers) -> do
              forMaybeM modifiers \case
                CanSpendUsesAsResourceOnCardFromInvestigator assetId uType iMatcher cMatcher -> do
                  canContribute <-
                    andM
                      [ liftA2 (&&) (elem iid <$> select iMatcher) (pure $ cardMatch card cMatcher)
                      , withoutModifier iid' CannotAffectOtherPlayersWithPlayerEffectsExceptDamage
                      ]
                  if canContribute
                    then do
                      total <- fieldMap AssetUses (findWithDefault 0 uType) assetId
                      name <- fieldMap AssetName toTitle assetId
                      pure $ guard (total > 0) $> (iid', assetId, uType, name, total)
                    else pure Nothing
                _ -> pure Nothing

          if null canHelpPay && null resourcesFromAssets
            then push (SpendResources iid x)
            else do
              iidsWithResources <-
                forMaybeM (iid : map fst canHelpPay) \iid' -> do
                  resources <- getSpendableResources iid'
                  name <- toTitle <$> field InvestigatorName iid'
                  pure $ guard (resources > 0) $> (iid', name, resources)

              case iidsWithResources of
                [(iid', _, z)] | z >= x && null resourcesFromAssets -> push $ SpendResources iid' x
                _
                  | sum (map (\(_, _, z) -> z) iidsWithResources) == x && null resourcesFromAssets ->
                      pushAll $ map (\(iid', _, z) -> SpendResources iid' z) iidsWithResources
                _ -> do
                  rs1 <- getRandoms
                  rs2 <- getRandoms
                  push
                    $ Ask player
                    $ ChoosePaymentAmounts ("Pay " <> tshow x <> " resources") (Just $ TotalAmountTarget x)
                    $ map
                      ( \(choiceId, (iid', name, resources)) -> PaymentAmountChoice choiceId iid' 0 resources name (SpendResources iid' 1)
                      )
                      (zip rs1 iidsWithResources)
                    <> map
                      ( \(choiceId, (iid', assetId, uType, name, total)) ->
                          PaymentAmountChoice
                            choiceId
                            iid'
                            0
                            total
                            (tshow uType <> " from " <> name)
                            (SpendUses source (toTarget assetId) uType 1)
                      )
                      (zip rs2 resourcesFromAssets)
      withPayment $ ResourcePayment x
    AdditionalActionsCost -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      let currentlyPaid = countAdditionalActionPayments c.payments
      pushWhen (actionRemainingCount > 0)
        $ chooseOne
          player
          [ Label
              "Spend 1 additional action"
              [ pay AdditionalActionCost
              , PaidAbilityCost iid Nothing AdditionalActionPayment
              , msg
              ]
          , Label ("Done spending additional actions (" <> tshow currentlyPaid <> " spent so far)") []
          ]
      pure c
    AdditionalActionsCostThatReducesResourceCostBy n nested -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      let currentlyPaid = countAdditionalActionPayments c.payments
      case c.target of
        ForCard _ _ -> do
          let
            go = \case
              ResourceCost x -> ResourceCost (max 0 (x - n))
              x -> x
            reduceResourceCost = transform go
            lockedActions = totalActionCost nested
          canAffordNested <-
            withAlteredGame withoutCanModifiers $ getCanAffordCost iid source actions c.windows nested
          if actionRemainingCount > lockedActions
            then
              push
                $ chooseOrRunOne player
                $ Label
                  "Spend 1 additional action"
                  [ pay AdditionalActionCost
                  , PaidAbilityCost iid Nothing AdditionalActionPayment
                  , PayCost acId iid skipAdditionalCosts
                      $ AdditionalActionsCostThatReducesResourceCostBy n
                      $ reduceResourceCost nested
                  ]
                : [ Label
                    ("Done spending additional actions (" <> tshow currentlyPaid <> " spent so far)")
                    [PayCost acId iid skipAdditionalCosts nested]
                  | canAffordNested
                  ]
            else push $ PayCost acId iid skipAdditionalCosts nested
        _ -> error "Unhandled active cost target for AdditionalActionsCostThatReducesResourceCostBy"
      pure c
    ActionCost x -> do
      costModifier' <- if skipAdditionalCosts then pure 0 else getActionCostModifier c
      let
        modifiedActionCost = max 0 (x + costModifier')
        actions' = case c.target of
          ForAbility a -> a.actions
          ForCard IsPlayAction c' -> #play : c'.actions
          ForCard NotPlayAction c' -> c'.actions
          _ -> []
        source' = case activeCostTarget c of
          ForAbility a -> toSource a
          _ -> source
      push $ SpendActions iid source' actions' modifiedActionCost
      withPayment $ ActionPayment x
    AdditionalActionCost -> do
      let
        actions' = case c.target of
          ForAbility a -> a.actions
          _ -> []
        source' = case activeCostTarget c of
          ForAbility a -> toSource a
          _ -> source
      push $ SpendActions iid source' actions' 1
      withPayment AdditionalActionPayment
    UseCost assetMatcher uType n -> do
      assets <- select $ assetMatcher <> AssetWithSpendableUses (atLeast n) uType
      push
        $ chooseOrRunOne player [targetLabel aid [SpendUses source (AssetTarget aid) uType n] | aid <- assets]
      withPayment $ UsesPayment n
    EventUseCost eventMatcher uType n -> do
      events <- select eventMatcher
      push
        $ chooseOrRunOne player [targetLabel eid [SpendUses source (EventTarget eid) uType n] | eid <- events]
      withPayment $ UsesPayment n
    DynamicUseCost assetMatcher uType costValue -> case costValue of
      DrawnCardsValue -> do
        let
          getDrawnCards [] = error "can not find drawn card in windows"
          getDrawnCards (x : xs) = case x of
            (windowType -> Window.DrawCards _ cards) -> length cards
            _ -> getDrawnCards xs
          n = getDrawnCards c.windows
        assets <- select assetMatcher
        push
          $ chooseOrRunOne player [targetLabel aid [SpendUses source (AssetTarget aid) uType n] | aid <- assets]
        withPayment $ UsesPayment n
    UseCostUpTo assetMatcher uType n m -> do
      assets <- select assetMatcher
      uses <- sum <$> traverse (fieldMap AssetUses (findWithDefault 0 uType)) assets
      let maxUses = min uses m

      name <- fieldMap InvestigatorName toTitle iid
      choiceId <- getRandom

      push
        $ Ask player
        $ ChoosePaymentAmounts
          ("Pay " <> displayCostType cost)
          Nothing
          [ PaymentAmountChoice choiceId iid n maxUses name
              $ pay (UseCost assetMatcher uType 1)
          ]
      pure c
    AssetClueCost _ aMatcher gv -> do
      totalClues <- getPlayerCountValue gv
      assets <- select $ aMatcher <> AssetWithAnyClues
      let
        source' =
          case c.target of
            ForAbility a -> toSource a
            _ -> source
      case assets of
        [] -> error "can not pay cost"
        [x] -> do
          push $ RemoveClues source' (toTarget x) totalClues
          withPayment $ CluePayment iid totalClues
        _ -> error "unhandled assumed to be from a single asset"
    ClueCost gv -> do
      totalClues <- getPlayerCountValue gv
      push $ InvestigatorSpendClues iid totalClues
      withPayment $ CluePayment iid totalClues
    ClueCostX -> do
      mVal <- getSpendableClueCount [iid]
      if mVal == 1
        then push $ pay (ClueCost (Static 1))
        else
          push
            $ questionLabel ("Spend 1-" <> tshow mVal <> " clues, as a group") player
            $ DropDown
              [ (tshow n, pay (ClueCost (Static n)))
              | n <- [1 .. mVal]
              ]
      pure c
    GroupClueCostX -> do
      mVal <- getSpendableClueCount =<< select UneliminatedInvestigator
      x <- perPlayer 1
      if mVal == x
        then push $ pay (GroupClueCost (PerPlayer 1) Anywhere)
        else
          push
            $ questionLabel ("Spend 1-" <> tshow mVal <> " {perPlayer} clues, as a group") player
            $ DropDown
              [ (tshow n, pay (GroupClueCost (PerPlayer n) Anywhere))
              | n <- [1 .. (mVal `div` x)]
              ]
      pure c
    PlaceClueOnLocationCost x -> do
      push $ InvestigatorPlaceCluesOnLocation iid source x
      withPayment $ CluePayment iid x
    GroupClueCostRange (sVal, eVal) locationMatcher -> do
      mVal <- min eVal . getSum <$> selectAgg Sum InvestigatorClues (InvestigatorAt locationMatcher)
      if mVal == sVal
        then push $ pay (GroupClueCost (Static sVal) locationMatcher)
        else
          push
            $ questionLabel ("Spend " <> tshow sVal <> "-" <> tshow mVal <> " clues, as a group") player
            $ DropDown
              [ (tshow n, pay (GroupClueCost (Static n) locationMatcher))
              | n <- [sVal .. mVal]
              ]
      pure c
    GroupClueCost x locationMatcher -> do
      totalClues <- getPlayerCountValue x
      iids <- select $ InvestigatorAt locationMatcher <> InvestigatorWithAnyClues
      iidsWithClues <- forMaybeM iids \iid' -> do
        clues <- getSpendableClueCount [iid']
        if clues > 0
          then do
            name <- fieldMap InvestigatorName toTitle iid'
            pure $ Just (iid', name, clues)
          else pure Nothing
      case iidsWithClues of
        [(iid', _, _)] -> push (PayCost acId iid' True (ClueCost (Static totalClues)))
        xs -> do
          if sum (map (\(_, _, x') -> x') xs) == totalClues
            then do
              for_ xs \(iid', _, cCount) -> push (PayCost acId iid' True (ClueCost (Static cCount)))
            else do
              rs <- getRandoms
              let
                paymentOptions =
                  map
                    ( \(choiceId, (iid', name, clues)) ->
                        PaymentAmountChoice choiceId iid' 0 clues name $ PayCost acId iid' True (ClueCost (Static 1))
                    )
                    (zip rs iidsWithClues)
              lead <- getLeadPlayer
              push
                $ Ask lead
                $ ChoosePaymentAmounts (displayCostType cost) (Just $ TotalAmountTarget totalClues) paymentOptions
      pure c
    -- push (SpendClues totalClues iids)
    -- withPayment $ CluePayment totalClues
    HandDiscardCost x extendedCardMatcher -> do
      handCards <- fieldMap InvestigatorHand (mapMaybe (preview _PlayerCard)) iid
      let
        notCostCard = case activeCostTarget c of
          ForAbility {} -> const True
          ForAdditionalCost {} -> const True
          ForCard _ card' -> (/= card')
          ForCost card' -> (/= card')
      cards <-
        filterM
          ( andM
              . sequence
                [ pure . notCostCard . PlayerCard
                , (`extendedCardMatch` (basic DiscardableCard <> extendedCardMatcher))
                ]
          )
          handCards
      push
        $ chooseN
          player
          x
          [ targetLabel
            card
            [pay (DiscardCardCost $ PlayerCard card)]
          | card <- cards
          ]
      pure c
    HandDiscardAnyNumberCost extendedCardMatcher -> do
      handCards <- fieldMap InvestigatorHand (mapMaybe (preview _PlayerCard)) iid
      let
        notCostCard = case activeCostTarget c of
          ForAbility {} -> const True
          ForAdditionalCost {} -> const True
          ForCard _ card' -> (/= card')
          ForCost card' -> (/= card')
      cards <-
        filterM
          (andM . sequence [pure . notCostCard . PlayerCard, (`extendedCardMatch` extendedCardMatcher)])
          handCards
      name <- fieldMap InvestigatorName toTitle iid
      choiceId <- getRandom
      push
        $ Ask player
        $ ChoosePaymentAmounts
          "Number of cards to pay"
          Nothing
          [ PaymentAmountChoice choiceId iid 1 (length cards) name
              $ pay (HandDiscardCost 1 extendedCardMatcher)
          ]
      pure c
    ReturnMatchingAssetToHandCost assetMatcher -> do
      assets <- select $ AssetCanLeavePlayByNormalMeans <> assetMatcher
      push $ chooseOne player $ targetLabels assets $ only . pay . ReturnAssetToHandCost
      pure c
    ReturnAssetToHandCost assetId -> do
      card <- field AssetCard assetId
      controller <- fieldMap AssetController (fromJustNote "Missing controller") assetId
      push $ ReturnToHand controller $ AssetTarget assetId
      withPayment $ ReturnToHandPayment card
    DiscardHandCost -> do
      handCards <- fieldMap InvestigatorHand (mapMaybe (preview _PlayerCard)) iid
      push $ DiscardHand iid c.source
      withPayment $ DiscardCardPayment $ map PlayerCard handCards
    DiscardFromCost x zone cardMatcher -> do
      let
        getCards = \case
          FromHandOf whoMatcher -> selectMap (FromHand,) (InHandOf whoMatcher <> BasicCardMatch cardMatcher)
          FromPlayAreaOf whoMatcher -> do
            assets <- select $ AssetControlledBy whoMatcher
            map (FromPlay,) . filter (`cardMatch` cardMatcher) <$> traverse (field AssetCard) assets
          CostZones zs -> concatMapM getCards zs
      cards <- getCards zone
      push
        $ chooseN
          player
          x
          [ targetLabel (toCardId card) [pay (DiscardCost zone' $ toTarget card)]
          | (zone', card) <- cards
          ]
      pure c
    SkillIconCost x skillTypes -> do
      handCards <- fieldMap InvestigatorHand (mapMaybe (preview _PlayerCard)) iid
      let countF = if null skillTypes then const True else (`member` insertSet WildIcon skillTypes)
      let
        cards =
          filter ((> 0) . fst)
            $ map (toFst (count countF . cdSkills . toCardDef)) handCards
        cardMsgs =
          map
            ( \(n, card) ->
                targetLabel (toCardId card)
                  $ toMessage (discardCard iid c.source card)
                  : PaidAbilityCost iid Nothing (SkillIconPayment card.skills)
                  : [pay (SkillIconCost (x - n) skillTypes) | n < x]
            )
            cards
      push $ chooseOne player cardMsgs
      pure c
    SameSkillIconCost x -> do
      handCards <- fieldMap InvestigatorHand (mapMaybe (preview _PlayerCard)) iid
      let total = unionsWith (+) $ map (frequencies . cdSkills . toCardDef) handCards
      let wildCount = total ^. at #wild . non 0
      let choices = keys $ filterMap (\n -> n + wildCount >= x) $ deleteMap #wild total
      push
        $ chooseOne
          player
          [ SkillLabel skill [pay (SkillIconCost x (singleton $ SkillIcon skill))]
          | SkillIcon skill <- choices
          ]
      pure c
    DiscardCombinedCost x -> do
      handCards <-
        fieldMap InvestigatorHand (mapMaybe (preview _PlayerCard) . filter (`cardMatch` NonWeakness)) iid
      let
        cards = map (toFst (maybe 0 toPrintedCost . cdCost . toCardDef)) handCards
        cardMsgs =
          map
            ( \(n, card) ->
                targetLabel (toCardId card)
                  $ toMessage (discardCard iid c.source card)
                  : PaidAbilityCost iid Nothing (DiscardCardPayment [PlayerCard card])
                  : [pay $ DiscardCombinedCost (x - n) | n < x]
            )
            cards
      push $ chooseOne player cardMsgs
      pure c
    ShuffleDiscardCost 0 _ -> pure c
    ShuffleDiscardCost n cardMatcher -> do
      cards <- fieldMap InvestigatorDiscard (map PlayerCard . filter (`cardMatch` cardMatcher)) iid
      let
        cardMsgs =
          map
            ( \card ->
                targetLabel
                  (toCardId card)
                  [ RemoveFromDiscard iid (toCardId card)
                  , ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
                  , PaidAbilityCost iid Nothing $ CardPayment card
                  , pay $ ShuffleDiscardCost (n - 1) cardMatcher
                  ]
            )
            cards
      pushAll [FocusCards cards, chooseOne player cardMsgs, UnfocusCards]
      pure c
    Free -> pure c
    UnpayableCost -> pure c

instance RunMessage ActiveCost where
  runMessage msg c = case msg of
    CheckAdditionalCosts acId | acId == c.id -> do
      mods <- getModifiers (ActiveCostTarget acId)
      let additionalCosts = fold [ac | AdditionalCost ac <- mods]
      pure $ c {activeCostCosts = activeCostCosts c <> additionalCosts}
    PayCosts acId | acId == c.id -> do
      push $ PayCost acId c.investigator False c.costs
      pure c
    CreatedCost acId | acId == c.id -> do
      let iid = c.investigator
      case c.target of
        ForAdditionalCost _ -> do
          pushAll [PayCosts acId, PayCostFinished acId]
          pure c
        ForCost _ -> do
          pushAll [PayCosts acId, PayCostFinished acId]
          pure c
        ForCard isPlayAction card -> do
          modifiers' <- (<>) <$> getModifiers iid <*> getModifiers card
          let cardDef = toCardDef card
          enabled <-
            createCardEffect
              cardDef
              (Just $ EffectCost acId)
              (BothSource (InvestigatorSource iid) (CardIdSource card.id))
              (toCardId card)

          let
            modifiersPreventAttackOfOpportunity = ActionDoesNotCauseAttacksOfOpportunity #play `elem` modifiers'
            actions = [Action.Play | isPlayAction == IsPlayAction] <> cardDef.actions
            mEffect =
              guard cardDef.beforeEffect
                *> [ enabled
                   , CheckAdditionalCosts acId
                   ]
          batchId <- getRandom
          beforeWindowMsg <- checkWindows $ map (mkWhen . Window.PerformAction iid) actions
          wouldPayWindowMsg <- checkWindows [mkWhen $ Window.WouldPayCardCost iid acId batchId card]
          -- We only need to check attacks of opportunity if we spend actions,
          -- indepdent of the card being fast (for example the card you would
          -- play off of Uncage the Soul)
          pushAll
            $ (guard (notNull actions) *> [BeginAction, beforeWindowMsg])
            <> mEffect
            <> [ wouldPayWindowMsg
               , Would
                  batchId
                  $ [PayCosts acId]
                  <> [ CheckAttackOfOpportunity iid False
                     | not modifiersPreventAttackOfOpportunity
                        && (DoesNotProvokeAttacksOfOpportunity `notElem` cardDef.attackOfOpportunityModifiers)
                        && isNothing cardDef.fastWindow
                        && all (`notElem` nonAttackOfOpportunityActions) actions
                        && (totalActionCost c.costs > 0)
                     ]
                  <> [PayCostFinished acId]
               ]
          pure c
        ForAbility a@Ability {..} -> do
          modifiers' <- getCombinedModifiers [toTarget iid, AbilityTarget iid a]
          let
            modifiersPreventAttackOfOpportunity =
              any ((`elem` modifiers') . ActionDoesNotCauseAttacksOfOpportunity) a.actions
          push $ PayCostFinished acId
          startAbilityPayment
            c
            iid
            (mkWhen Window.NonFast) -- TODO: a thing
            abilityType
            abilitySource
            (abilityDoesNotProvokeAttacksOfOpportunity || modifiersPreventAttackOfOpportunity)
          pure c
    PayCost acId iid skipAdditionalCosts cost | acId == c.id -> do
      let
        source = c.source
        actions = c.actions

      extraResources <- case activeCostTarget c of
        ForCard _ card -> do
          iids <- getInvestigators
          sum <$> for iids \iid' -> do
            modifiers <- getModifiers (InvestigatorTarget iid')
            sum <$> for modifiers \case
              CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher -> do
                canGive <-
                  andM [pure $ cardMatch card cMatcher, pure $ iid /= iid', elem iid <$> select iMatcher]
                if canGive then getSpendableResources iid' else pure 0
              CanSpendUsesAsResourceOnCardFromInvestigator assetId uType iMatcher cMatcher -> do
                canContribute <-
                  andM
                    [ pure $ cardMatch card cMatcher
                    , pure $ iid == iid' || CannotAffectOtherPlayersWithPlayerEffectsExceptDamage `notElem` modifiers
                    , iid <=~> iMatcher
                    ]
                if canContribute
                  then fieldMap AssetUses (findWithDefault 0 uType) assetId
                  else pure 0
              _ -> pure 0
        _ -> pure 0

      let
        canModify =
          case c.target of
            ForCard {} -> False
            _ -> True

      canStillAfford <-
        withModifiers iid (toModifiers source [ExtraResources extraResources])
          $ getCanAffordCost_ iid source actions c.windows canModify cost
      if canStillAfford
        then payCost msg c iid skipAdditionalCosts cost
        else do
          case c.target of
            ForAdditionalCost batchId -> do
              push $ IgnoreBatch batchId
              pure c
            _ -> do
              isParallelAgnes <- iid <=~> InvestigatorIs "90017"
              if isParallelAgnes
                then do
                  let cost' = decreaseResourceCost cost 2
                  canStillAfford' <-
                    withModifiers iid (toModifiers source [ExtraResources extraResources])
                      $ getCanAffordCost_ iid source actions c.windows canModify cost'
                  if canStillAfford'
                    then payCost msg c iid skipAdditionalCosts cost'
                    else throw $ InvalidState $ "Can't afford cost: " <> tshow cost'
                else throw $ InvalidState $ "Can't afford cost: " <> tshow cost
    SetCost acId cost | acId == c.id -> do
      pure $ c {activeCostCosts = cost}
    PaidCost acId _ _ payment | acId == c.id -> do
      pure $ c & costPaymentsL <>~ payment
    PayCostFinished acId | acId == c.id -> do
      case c.target of
        ForAbility ability -> do
          let
            isAction = isActionAbility ability && ability.index > 0
            actions = Action.Activate : ability.actions
            iid = c.investigator
          whenActivateAbilityWindow <- checkWindows [mkWhen (Window.ActivateAbility iid c.windows ability)]
          afterActivateAbilityWindow <- checkWindows [mkAfter (Window.ActivateAbility iid c.windows ability)]
          afterMsgs <-
            if isAction
              then do
                afterWindowMsgs <- checkWindows [mkAfter (Window.PerformAction iid action) | action <- actions]
                pure [afterWindowMsgs, FinishAction]
              else pure []
          -- TODO: this will not work for ForcedWhen, but this currently only applies to IntelReport
          isForced <- isForcedAbility iid ability
          card <- sourceToCard ability.source
          pushAll
            $ [whenActivateAbilityWindow | not isForced]
            <> [SealedChaosToken token (toTarget card) | token <- c.sealedChaosTokens]
            <> [UseCardAbility iid ability.source ability.index c.windows c.payments]
            <> afterMsgs
            <> [afterActivateAbilityWindow | not isForced]
        ForCard isPlayAction card -> do
          let iid = c.investigator
          let actions = [#play | isPlayAction == IsPlayAction] <> card.actions
          let ability = restricted iid PlayAbility (Self <> Never) (ActionAbility [#play] $ ActionCost 1)
          whenActivateAbilityWindow <- checkWindows [mkWhen (Window.ActivateAbility iid c.windows ability)]
          afterActivateAbilityWindow <- checkWindows [mkAfter (Window.ActivateAbility iid c.windows ability)]
          pushAll
            $ [whenActivateAbilityWindow | isPlayAction == IsPlayAction]
            <> [PlayCard iid card Nothing c.payments c.windows False]
            <> [SealedChaosToken token (toTarget card) | token <- c.sealedChaosTokens]
            <> [FinishAction | notNull actions]
            <> [TakenActions iid actions | notNull actions]
            <> [afterActivateAbilityWindow | isPlayAction == IsPlayAction]
        ForCost card -> pushAll [SealedChaosToken token (toTarget card) | token <- c.sealedChaosTokens]
        ForAdditionalCost _ -> pure ()
      push PaidAllCosts
      pure c
    _ -> pure c
