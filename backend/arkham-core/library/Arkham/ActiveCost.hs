{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.ActiveCost (
  module Arkham.ActiveCost,
  module X,
) where

import Arkham.Prelude

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
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Cost.FieldCost
import Arkham.Deck qualified as Deck
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.Enemy.Types (Field (..))
import Arkham.Event.Types (Field (..))
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Helpers.Message
import Arkham.Id
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
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Skill.Types (Field (..))
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token qualified as Token
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..), mkAfter, mkWhen, mkWindow)
import Arkham.Window qualified as Window

activeCostActions :: ActiveCost -> [Action]
activeCostActions ac = case activeCostTarget ac of
  ForAbility a -> Action.Activate : abilityActions a
  ForCard isPlayAction c -> [Action.Play | isPlayAction == IsPlayAction] <> cdActions (toCardDef c)
  ForCost _ -> []

addActiveCostCost :: Cost -> ActiveCost -> ActiveCost
addActiveCostCost cost ac = ac & costsL <>~ cost

activeCostSource :: ActiveCost -> Source
activeCostSource ac = case activeCostTarget ac of
  ForAbility a -> abilitySource a
  ForCard _ c -> CardSource c
  ForCost c -> CardSource c

costsL :: Lens' ActiveCost Cost
costsL = lens activeCostCosts $ \m x -> m {activeCostCosts = x}

costPaymentsL :: Lens' ActiveCost Payment
costPaymentsL = lens activeCostPayments $ \m x -> m {activeCostPayments = x}

costSealedChaosTokensL :: Lens' ActiveCost [ChaosToken]
costSealedChaosTokensL =
  lens activeCostSealedChaosTokens $ \m x -> m {activeCostSealedChaosTokens = x}

activeCostPaid :: ActiveCost -> Bool
activeCostPaid = (== Free) . activeCostCosts

matchTarget :: [[Action]] -> [[Action]] -> ActionTarget -> Action -> Bool
matchTarget _takenActions performedActions (FirstOneOfPerformed as) action =
  action `elem` as && all (\a -> all (notElem a) performedActions) as
matchTarget _ _ (IsAction a) action = action == a
matchTarget _ _ (EnemyAction a _) action = action == a
matchTarget _ _ IsAnyAction _ = True

getActionCostModifier :: HasGame m => ActiveCost -> m Int
getActionCostModifier ac = do
  let iid = activeCostInvestigator ac
  takenActions <- field InvestigatorActionsTaken iid
  performedActions <- field InvestigatorActionsPerformed iid
  modifiers <- getModifiers iid
  pure $ foldr (applyModifier takenActions performedActions) 0 modifiers
 where
  actions = case activeCostActions ac of
    [] -> error "expected action"
    as -> as
  applyModifier takenActions performedActions (ActionCostOf match m) n =
    -- For cards we've already calculated the cost as an additional cost for
    -- the action specifically
    case activeCostTarget ac of
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
startAbilityPayment activeCost@ActiveCost {activeCostId} iid window abilityType abilitySource abilityDoesNotProvokeAttacksOfOpportunity =
  case abilityType of
    Objective aType ->
      startAbilityPayment
        activeCost
        iid
        window
        aType
        abilitySource
        abilityDoesNotProvokeAttacksOfOpportunity
    ForcedAbility _ -> pure ()
    SilentForcedAbility _ -> pure ()
    Haunted -> pure ()
    Cosmos -> pure ()
    ForcedAbilityWithCost {} -> push (PayCosts activeCostId)
    AbilityEffect {} -> push (PayCosts activeCostId)
    FastAbility' _ mAction ->
      pushAll
        $ PayCosts activeCostId
        : [PerformedActions iid [action] | action <- toList mAction]
    ForcedWhen _ aType ->
      startAbilityPayment
        activeCost
        iid
        window
        aType
        abilitySource
        abilityDoesNotProvokeAttacksOfOpportunity
    ReactionAbility {} -> push (PayCosts activeCostId)
    ActionAbilityWithBefore actions' _ cost -> do
      -- we do not know which ability will be chosen
      -- for now we assume this will trigger attacks of opportunity
      -- we also skip additional cost checks and abilities of this type
      -- will need to trigger the appropriate check
      let actions = Action.Activate : actions'
      -- TODO: move to PayCosts
      pushAll
        $ [ PayCost activeCostId iid True cost
          , TakenActions iid actions
          ]
        <> [ CheckAttackOfOpportunity iid False
           | not abilityDoesNotProvokeAttacksOfOpportunity
           ]
    ActionAbilityWithSkill actions' _ _ -> do
      let actions = Action.Activate : actions'
      if all (`notElem` [#fight, #evade, #resign, #parley]) actions
        then
          pushAll
            $ PayCosts activeCostId
            : [TakenActions iid actions]
              <> [ CheckAttackOfOpportunity iid False
                 | not abilityDoesNotProvokeAttacksOfOpportunity
                 ]
        else
          pushAll
            [ PayCosts activeCostId
            , TakenActions iid actions
            ]
    ActionAbility actions' _ -> do
      let actions = Action.Activate : actions'
      beforeWindowMsg <- checkWindows [mkWhen $ Window.PerformAction iid action | action <- actions]
      if all (`notElem` [Action.Fight, Action.Evade, Action.Resign, Action.Parley]) actions
        then
          pushAll
            $ [ BeginAction
              , beforeWindowMsg
              , PayCosts activeCostId
              , TakenActions iid actions
              ]
            <> [ CheckAttackOfOpportunity iid False
               | not abilityDoesNotProvokeAttacksOfOpportunity
               ]
        else
          pushAll
            $ [ BeginAction
              , beforeWindowMsg
              , PayCosts activeCostId
              , TakenActions iid actions
              ]

nonAttackOfOpportunityActions :: [Action]
nonAttackOfOpportunityActions = [Action.Fight, Action.Evade, Action.Resign, Action.Parley]

payCost
  :: (HasGame m, HasQueue Message m)
  => Message
  -> ActiveCost
  -> InvestigatorId
  -> Bool
  -> Cost
  -> m ActiveCost
payCost msg c iid skipAdditionalCosts cost = do
  let acId = activeCostId c
  let withPayment payment = pure $ c & costPaymentsL <>~ payment
  let source = activeCostSource c
  let
    actions =
      case activeCostActions c of
        [] -> error "action expected"
        as -> as
  player <- getPlayer iid
  case cost of
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
      hauntedAbilities <-
        selectList
          $ HauntedAbility
          <> AbilityOnLocation
            (LocationWithId lid)
      when (notNull hauntedAbilities)
        $ push
        $ chooseOneAtATime
          player
          [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]
      -- No need to record payment... yet
      pure c
    OrCost xs -> do
      push
        $ chooseOne player
        $ map
          ( \x ->
              Label
                (displayCostType x)
                [PayCost acId iid skipAdditionalCosts x]
          )
          xs
      pure c
    OptionalCost x -> do
      canAfford <- getCanAffordCost iid source actions [] x
      pushWhen canAfford
        $ chooseOne
          player
          [ Label (displayCostType x) [PayCost acId iid skipAdditionalCosts x]
          , Label "Do not pay" []
          ]
      pure c
    Costs xs ->
      c <$ pushAll [PayCost acId iid skipAdditionalCosts x | x <- xs]
    UpTo 0 _ -> pure c
    UpTo n cost' -> do
      canAfford <-
        andM
          $ map
            (\a -> getCanAffordCost iid source [a] [] cost')
            actions
      maxUpTo <- case cost' of
        ResourceCost resources -> do
          availableResources <- getSpendableResources iid
          pure $ min n (availableResources `div` resources)
        _ -> pure n
      name <- fieldMap InvestigatorName toTitle iid
      when canAfford
        $ push
        $ Ask player
        $ ChoosePaymentAmounts
          ("Pay " <> displayCostType cost)
          Nothing
          [ PaymentAmountChoice iid 0 maxUpTo name
              $ PayCost acId iid skipAdditionalCosts cost'
          ]
      pure c
    DiscardTopOfDeckCost n -> do
      cards <-
        fieldMap
          InvestigatorDeck
          (map PlayerCard . take n . unDeck)
          iid
      push $ DiscardTopOfDeck iid n source Nothing
      withPayment $ DiscardCardPayment cards
    IncreaseCostOfThis cardId n -> do
      push
        $ CreateWindowModifierEffect
          (EffectCardCostWindow cardId)
          (EffectModifiers $ toModifiers source [IncreaseCostOf (CardWithId cardId) n])
          source
          (toTarget cardId)
      pure c
    ExhaustCost target -> do
      push (Exhaust target)
      withPayment $ ExhaustPayment [target]
    ExhaustAssetCost matcher -> do
      targets <- map AssetTarget <$> selectList (matcher <> AssetReady)
      c
        <$ push
          ( chooseOne
              player
              [ TargetLabel
                target
                [PayCost acId iid skipAdditionalCosts (ExhaustCost target)]
              | target <- targets
              ]
          )
    SealCost matcher -> do
      targets <-
        filterM (\t -> matchChaosToken iid t matcher)
          =<< scenarioFieldMap ScenarioChaosBag chaosBagChaosTokens
      pushAll
        [ FocusChaosTokens targets
        , chooseOne
            player
            [ TargetLabel
              (ChaosTokenTarget target)
              [PayCost acId iid skipAdditionalCosts (SealChaosTokenCost target)]
            | target <- targets
            ]
        , UnfocusChaosTokens
        ]
      pure c
    SealChaosTokenCost token -> do
      push $ SealChaosToken token
      pure
        $ c
        & costPaymentsL
        <>~ SealChaosTokenPayment token
        & costSealedChaosTokensL
        %~ (token :)
    ReleaseChaosTokensCost n -> do
      case source of
        AssetSource aid -> do
          tokens <- field AssetSealedChaosTokens aid
          pushAll
            $ [ FocusChaosTokens tokens
              , chooseN
                  player
                  n
                  [ TargetLabel
                    (ChaosTokenTarget t)
                    [ PayCost
                        acId
                        iid
                        skipAdditionalCosts
                        (ReleaseChaosTokenCost t)
                    ]
                  | t <- tokens
                  ]
              , UnfocusChaosTokens
              ]
        _ -> error "Unhandled source for releasing tokens cost"
      pure c
    ReleaseChaosTokenCost t -> do
      push $ UnsealChaosToken t
      pure $ c & (costPaymentsL <>~ ReleaseChaosTokenPayment t)
    SupplyCost matcher supply -> do
      iid' <-
        selectJust $ InvestigatorWithSupply supply <> InvestigatorAt matcher
      push $ UseSupply iid' supply
      withPayment $ SupplyPayment supply
    DiscardCost zone target -> do
      card <- targetToCard target
      pushAll [DiscardedCost target, toDiscardBy iid (activeCostSource c) target]
      withPayment $ DiscardPayment [(zone, card)]
    DiscardAssetCost matcher -> do
      targets <- map AssetTarget <$> selectList (matcher <> AssetReady)
      push
        ( chooseOne
            player
            [ TargetLabel
              target
              [PayCost acId iid skipAdditionalCosts (DiscardCost FromPlay target)]
            | target <- targets
            ]
        )
      pure c
    DiscardRandomCardCost -> do
      push $ toMessage $ randomDiscard iid (activeCostSource c)
      pure c
    DiscardCardCost card -> do
      push $ toMessage $ discardCard iid (activeCostSource c) card
      withPayment $ DiscardCardPayment [card]
    DiscardDrawnCardCost -> do
      let
        getDrawnCard [] = error "can not find drawn card in windows"
        getDrawnCard (x : xs) = case x of
          (windowType -> Window.DrawCard _ card' _) -> card'
          _ -> getDrawnCard xs
        card = getDrawnCard (activeCostWindows c)
      push $ toMessage $ discardCard iid (activeCostSource c) card
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
      enemies <- selectListMap EnemyTarget matcher
      push
        $ chooseOrRunOne
          player
          [TargetLabel target [PlaceDoom source target x] | target <- enemies]
      withPayment $ DoomPayment x
    DoomCost _ (AgendaMatcherTarget matcher) x -> do
      agendas <- selectListMap AgendaTarget matcher
      pushAll [PlaceDoom source target x | target <- agendas]
      withPayment $ DoomPayment (x * length agendas)
    DoomCost _ target x -> do
      push (PlaceDoom source target x)
      withPayment $ DoomPayment x
    HorrorCost _ target x -> case target of
      InvestigatorTarget iid' | iid' == iid -> do
        push (InvestigatorAssignDamage iid source DamageAny 0 x)
        withPayment $ HorrorPayment x
      YouTarget -> do
        push (InvestigatorAssignDamage iid source DamageAny 0 x)
        withPayment $ HorrorPayment x
      AssetTarget aid -> do
        push $ AssetDamage aid source 0 x
        withPayment $ HorrorPayment x
      _ -> error "can't target for horror cost"
    HorrorCostX source' -> do
      -- see: The Black Book as that is the only card that uses this
      let
        getPlayedCard [] = error "can not find played card in windows"
        getPlayedCard (x : xs) = case x of
          (windowType -> Window.PlayCard _ card') -> card'
          _ -> getPlayedCard xs
        card = getPlayedCard (activeCostWindows c)
      availableResources <- getSpendableResources iid
      requiredResources <- getModifiedCardCost iid card
      let minimumHorror = max 1 (requiredResources - availableResources)
      sanity <- field InvestigatorRemainingSanity iid
      name <- fieldMap InvestigatorName toTitle iid
      push
        $ Ask player
        $ ChoosePaymentAmounts
          "Pay X Horror"
          Nothing
          [ PaymentAmountChoice iid minimumHorror sanity name
              $ PayCost acId iid skipAdditionalCosts (HorrorCost source' (InvestigatorTarget iid) 1)
          ]
      pure c
    DamageCost _ target x -> case target of
      InvestigatorTarget iid' | iid' == iid -> do
        push (InvestigatorAssignDamage iid source DamageAny x 0)
        withPayment $ DamagePayment x
      YouTarget -> do
        push (InvestigatorAssignDamage iid source DamageAny x 0)
        withPayment $ DamagePayment x
      AssetTarget aid -> do
        push $ AssetDamage aid source x 0
        withPayment $ DamagePayment x
      _ -> error "can't target for damage cost"
    DirectDamageCost _ investigatorMatcher x -> do
      investigators <- selectList investigatorMatcher
      case investigators of
        [iid'] -> do
          push $ InvestigatorDirectDamage iid' source x 0
          withPayment $ DirectDamagePayment x
        _ -> error "exactly one investigator expected for direct damage"
    InvestigatorDamageCost source' investigatorMatcher damageStrategy x -> do
      investigators <- selectList investigatorMatcher
      push
        $ chooseOrRunOne
          player
          [ targetLabel
            iid'
            [InvestigatorAssignDamage iid' source' damageStrategy x 0]
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
          push
            $ Ask player
            $ ChoosePaymentAmounts
              ("Pay X resources")
              (Just $ AmountOneOf ns)
              [PaymentAmountChoice iid 0 resources name (PayCost acId iid True (ResourceCost 1))]
      pure c
    MaybeFieldResourceCost (MaybeFieldCost mtchr fld) -> do
      ns <- nub . catMaybes <$> selectFields fld mtchr
      case ns of
        [] -> pure ()
        [n] -> push $ PayCost acId iid True (ResourceCost n)
        _ -> do
          resources <- getSpendableResources iid
          name <- fieldMap InvestigatorName toTitle iid
          push
            $ Ask player
            $ ChoosePaymentAmounts
              ("Pay X resources")
              (Just $ AmountOneOf ns)
              [PaymentAmountChoice iid 0 resources name (PayCost acId iid True (ResourceCost 1))]
      pure c
    ScenarioResourceCost n -> do
      push $ RemoveTokens (activeCostSource c) ScenarioTarget Token.Resource n
      withPayment $ ResourcePayment n
    AddCurseTokenCost n -> do
      pushAll $ replicate n $ AddChaosToken CurseToken
      pure c
    ResourceCost x -> do
      case activeCostTarget c of
        ForAbility {} -> push $ SpendResources iid x
        ForCost {} -> push $ SpendResources iid x
        ForCard _ card -> do
          iids <- getInvestigatorIds
          iidsWithModifiers <- for iids $ \iid' -> do
            modifiers <- getModifiers (InvestigatorTarget iid')
            pure (iid', modifiers)
          canHelpPay <- flip filterM iidsWithModifiers $ \(iid', modifiers) ->
            do
              flip anyM modifiers $ \case
                CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher ->
                  andM
                    [ member iid <$> select iMatcher
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
                      [ liftA2 (&&) (member iid <$> select iMatcher) (pure $ cardMatch card cMatcher)
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
                for (iid : map fst canHelpPay) \iid' -> do
                  resources <- getSpendableResources iid'
                  name <- toTitle <$> field InvestigatorName iid'
                  pure (iid', name, resources)

              push
                $ Ask player
                $ ChoosePaymentAmounts
                  ("Pay " <> tshow x <> " resources")
                  (Just $ TotalAmountTarget x)
                $ map
                  ( \(iid', name, resources) ->
                      PaymentAmountChoice
                        iid'
                        0
                        resources
                        name
                        (SpendResources iid' 1)
                  )
                  iidsWithResources
                <> map
                  ( \(iid', assetId, uType, name, total) ->
                      PaymentAmountChoice
                        iid'
                        0
                        total
                        (tshow uType <> " from " <> name)
                        (SpendUses (toTarget assetId) uType 1)
                  )
                  resourcesFromAssets
      withPayment $ ResourcePayment x
    AdditionalActionsCost -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      let
        currentlyPaid =
          countAdditionalActionPayments (activeCostPayments c)
      c
        <$ if actionRemainingCount == 0
          then pure ()
          else
            push
              ( chooseOne
                  player
                  [ Label
                      "Spend 1 additional action"
                      [ PayCost acId iid skipAdditionalCosts (ActionCost 1)
                      , PaidAbilityCost iid Nothing AdditionalActionPayment
                      , msg
                      ]
                  , Label
                      ( "Done spending additional actions ("
                          <> tshow currentlyPaid
                          <> " spent so far)"
                      )
                      []
                  ]
              )
    ActionCost x -> do
      costModifier' <-
        if skipAdditionalCosts
          then pure 0
          else getActionCostModifier c
      let
        modifiedActionCost = max 0 (x + costModifier')
        actions' = case activeCostTarget c of
          ForAbility a -> abilityActions a
          _ -> []
        source' = case activeCostTarget c of
          ForAbility a -> AbilitySource (abilitySource a) (abilityIndex a)
          _ -> source
      push (SpendActions iid source' actions' modifiedActionCost)
      withPayment $ ActionPayment x
    UseCost assetMatcher uType n -> do
      assets <- selectList assetMatcher
      push
        $ chooseOrRunOne
          player
          [ TargetLabel
            (AssetTarget aid)
            [SpendUses (AssetTarget aid) uType n]
          | aid <- assets
          ]
      withPayment $ UsesPayment n
    DynamicUseCost assetMatcher uType costValue -> case costValue of
      DrawnCardsValue -> do
        let
          getDrawnCards [] = error "can not find drawn card in windows"
          getDrawnCards (x : xs) = case x of
            (windowType -> Window.DrawCards _ cards) -> length cards
            _ -> getDrawnCards xs
          n = getDrawnCards (activeCostWindows c)
        assets <- selectList assetMatcher
        push
          $ chooseOrRunOne
            player
            [ TargetLabel
              (AssetTarget aid)
              [SpendUses (AssetTarget aid) uType n]
            | aid <- assets
            ]
        withPayment $ UsesPayment n
    UseCostUpTo assetMatcher uType n m -> do
      assets <- selectList assetMatcher
      uses <-
        sum <$> traverse (fieldMap AssetUses (findWithDefault 0 uType)) assets
      let maxUses = min uses m

      name <- fieldMap InvestigatorName toTitle iid

      push
        $ Ask player
        $ ChoosePaymentAmounts
          ("Pay " <> displayCostType cost)
          Nothing
          [ PaymentAmountChoice iid n maxUses name
              $ PayCost
                acId
                iid
                skipAdditionalCosts
                (UseCost assetMatcher uType 1)
          ]
      pure c
    AssetClueCost _ aMatcher gv -> do
      totalClues <- getPlayerCountValue gv
      assets <- selectList $ aMatcher <> AssetWithAnyClues
      let
        source' =
          case activeCostTarget c of
            ForAbility a -> AbilitySource (abilitySource a) (abilityIndex a)
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
        then push $ PayCost acId iid skipAdditionalCosts (ClueCost (Static 1))
        else
          push
            $ questionLabel ("Spend 1-" <> tshow mVal <> " clues, as a group") player
            $ DropDown
              [ (tshow n, PayCost acId iid skipAdditionalCosts (ClueCost (Static n)))
              | n <- [1 .. mVal]
              ]
      pure c
    PlaceClueOnLocationCost x -> do
      push $ InvestigatorPlaceCluesOnLocation iid source x
      withPayment $ CluePayment iid x
    GroupClueCostRange (sVal, eVal) locationMatcher -> do
      mVal <- min eVal . getSum <$> selectAgg Sum InvestigatorClues (InvestigatorAt locationMatcher)
      if mVal == sVal
        then push $ PayCost acId iid skipAdditionalCosts (GroupClueCost (Static sVal) locationMatcher)
        else
          push
            $ questionLabel ("Spend " <> tshow sVal <> "-" <> tshow mVal <> " clues, as a group") player
            $ DropDown
              [ (tshow n, PayCost acId iid skipAdditionalCosts (GroupClueCost (Static n) locationMatcher))
              | n <- [sVal .. mVal]
              ]
      pure c
    GroupClueCost x locationMatcher -> do
      totalClues <- getPlayerCountValue x
      iids <-
        selectList
          $ InvestigatorAt locationMatcher
          <> InvestigatorWithAnyClues
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
          if (sum (map (\(_, _, x') -> x') xs) == totalClues)
            then do
              for_ xs \(iid', _, cCount) -> push (PayCost acId iid' True (ClueCost (Static cCount)))
            else do
              let
                paymentOptions =
                  map
                    ( \(iid', name, clues) ->
                        PaymentAmountChoice
                          iid'
                          0
                          clues
                          name
                          (PayCost acId iid' True (ClueCost (Static 1)))
                    )
                    iidsWithClues
              lead <- getLeadPlayer
              push
                $ Ask lead
                $ ChoosePaymentAmounts
                  (displayCostType cost)
                  (Just $ TotalAmountTarget totalClues)
                  paymentOptions
      pure c
    -- push (SpendClues totalClues iids)
    -- withPayment $ CluePayment totalClues
    HandDiscardCost x cardMatcher -> do
      handCards <-
        fieldMap
          InvestigatorHand
          (mapMaybe (preview _PlayerCard))
          iid
      let
        notCostCard = case activeCostTarget c of
          ForAbility {} -> const True
          ForCard _ card' -> (/= card')
          ForCost card' -> (/= card')
        cards =
          filter
            ( and
                . sequence
                  [(`cardMatch` cardMatcher), notCostCard . PlayerCard]
            )
            handCards
      push
        $ chooseN
          player
          x
          [ targetLabel
            (toCardId card)
            [ PayCost
                acId
                iid
                skipAdditionalCosts
                (DiscardCardCost $ PlayerCard card)
            ]
          | card <- cards
          ]
      pure c
    HandDiscardAnyNumberCost cardMatcher -> do
      handCards <-
        fieldMap
          InvestigatorHand
          (mapMaybe (preview _PlayerCard))
          iid
      let
        notCostCard = case activeCostTarget c of
          ForAbility {} -> const True
          ForCard _ card' -> (/= card')
          ForCost card' -> (/= card')
        cards =
          filter
            ( and
                . sequence
                  [(`cardMatch` cardMatcher), notCostCard . PlayerCard]
            )
            handCards
      name <- fieldMap InvestigatorName toTitle iid
      push
        $ Ask player
        $ ChoosePaymentAmounts
          "Number of cards to pay"
          Nothing
          [ PaymentAmountChoice iid 1 (length cards) name
              $ PayCost acId iid skipAdditionalCosts (HandDiscardCost 1 cardMatcher)
          ]
      pure c
    ReturnMatchingAssetToHandCost assetMatcher -> do
      assets <- selectList assetMatcher
      push
        $ chooseOne
          player
          [ targetLabel
            asset
            [ PayCost
                acId
                iid
                skipAdditionalCosts
                (ReturnAssetToHandCost asset)
            ]
          | asset <- assets
          ]
      pure c
    ReturnAssetToHandCost assetId -> do
      card <- field AssetCard assetId
      controller <-
        fieldMap
          AssetController
          (fromJustNote "Missing controller")
          assetId
      push $ ReturnToHand controller $ AssetTarget assetId
      withPayment $ ReturnToHandPayment card
    DiscardHandCost -> do
      handCards <-
        fieldMap
          InvestigatorHand
          (mapMaybe (preview _PlayerCard))
          iid
      push $ DiscardHand iid (activeCostSource c)
      withPayment $ DiscardCardPayment $ map PlayerCard handCards
    DiscardFromCost x zone cardMatcher -> do
      let
        getCards = \case
          FromHandOf whoMatcher ->
            selectListMap
              (FromHand,)
              (InHandOf whoMatcher <> BasicCardMatch cardMatcher)
          FromPlayAreaOf whoMatcher -> do
            assets <- selectList $ AssetControlledBy whoMatcher
            map (FromPlay,)
              . filter (`cardMatch` cardMatcher)
              <$> traverse (field AssetCard) assets
          CostZones zs -> concatMapM getCards zs
      cards <- getCards zone
      c
        <$ push
          ( chooseN
              player
              x
              [ targetLabel
                (toCardId card)
                [ PayCost
                    acId
                    iid
                    skipAdditionalCosts
                    (DiscardCost zone' $ CardTarget card)
                ]
              | (zone', card) <- cards
              ]
          )
    SkillIconCost x skillTypes -> do
      handCards <-
        fieldMap
          InvestigatorHand
          (mapMaybe (preview _PlayerCard))
          iid
      let
        cards =
          filter ((> 0) . fst)
            $ map
              ( toFst
                  ( count (`member` insertSet WildIcon skillTypes)
                      . cdSkills
                      . toCardDef
                  )
              )
              handCards
        cardMsgs =
          map
            ( \(n, card) ->
                if n >= x
                  then
                    targetLabel
                      (toCardId card)
                      [ toMessage $ discardCard iid (activeCostSource c) card
                      , PaidAbilityCost
                          iid
                          Nothing
                          (SkillIconPayment $ cdSkills $ toCardDef card)
                      ]
                  else
                    targetLabel
                      (toCardId card)
                      [ toMessage $ discardCard iid (activeCostSource c) card
                      , PaidAbilityCost
                          iid
                          Nothing
                          (SkillIconPayment $ cdSkills $ toCardDef card)
                      , PayCost
                          acId
                          iid
                          skipAdditionalCosts
                          (SkillIconCost (x - n) skillTypes)
                      ]
            )
            cards
      c <$ push (chooseOne player cardMsgs)
    DiscardCombinedCost x -> do
      handCards <-
        fieldMap
          InvestigatorHand
          (mapMaybe (preview _PlayerCard) . filter (`cardMatch` NonWeakness))
          iid
      let
        cards =
          map (toFst (maybe 0 toPrintedCost . cdCost . toCardDef)) handCards
        cardMsgs =
          map
            ( \(n, card) ->
                if n >= x
                  then
                    targetLabel
                      (toCardId card)
                      [ toMessage $ discardCard iid (activeCostSource c) card
                      , PaidAbilityCost iid Nothing
                          $ DiscardCardPayment [PlayerCard card]
                      ]
                  else
                    targetLabel
                      (toCardId card)
                      [ toMessage $ discardCard iid (activeCostSource c) card
                      , PaidAbilityCost iid Nothing
                          $ DiscardCardPayment [PlayerCard card]
                      , PayCost acId iid skipAdditionalCosts
                          $ DiscardCombinedCost (x - n)
                      ]
            )
            cards
      c <$ push (chooseOne player cardMsgs)
    ShuffleDiscardCost 0 _ -> pure c
    ShuffleDiscardCost n cardMatcher -> do
      cards <-
        fieldMap
          InvestigatorDiscard
          (map PlayerCard . filter (`cardMatch` cardMatcher))
          iid
      let
        cardMsgs =
          map
            ( \card ->
                targetLabel
                  (toCardId card)
                  [ RemoveFromDiscard iid (toCardId card)
                  , ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
                  , PaidAbilityCost iid Nothing $ CardPayment card
                  , PayCost acId iid skipAdditionalCosts
                      $ ShuffleDiscardCost (n - 1) cardMatcher
                  ]
            )
            cards
      c <$ pushAll [FocusCards cards, chooseOne player cardMsgs, UnfocusCards]
    Free -> pure c

instance RunMessage ActiveCost where
  runMessage msg c = case msg of
    PayCosts acId | acId == activeCostId c -> do
      let iid = activeCostInvestigator c
      push $ PayCost acId iid False (activeCostCosts c)
      pure c
    CreatedCost acId | acId == activeCostId c -> do
      let iid = activeCostInvestigator c
      case activeCostTarget c of
        ForCost _ -> do
          pushAll [PayCosts acId, PayCostFinished acId]
          pure c
        ForCard isPlayAction card -> do
          modifiers' <- getModifiers (InvestigatorTarget iid)
          let
            cardDef = toCardDef card
            modifiersPreventAttackOfOpportunity =
              ActionDoesNotCauseAttacksOfOpportunity Action.Play
                `elem` modifiers'
            actions = [Action.Play | isPlayAction == IsPlayAction] <> cdActions cardDef
            mEffect =
              guard (cdBeforeEffect cardDef) $> createCardEffect cardDef Nothing iid (toCardId card)
          batchId <- getRandom
          beforeWindowMsg <- checkWindows $ map (mkWhen . Window.PerformAction iid) actions
          wouldPayWindowMsg <- checkWindows [mkWhen $ Window.WouldPayCardCost iid acId batchId card]
          -- We only need to check attacks of opportunity if we spend actions,
          -- indepdent of the card being fast (for example the card you would
          -- play off of Uncage the Soul)
          let totalActions = totalActionCost (activeCostCosts c)
          pushAll
            $ ( guard (notNull actions)
                  *> [ BeginAction
                     , beforeWindowMsg
                     ]
              )
            <> maybeToList mEffect
            <> [ wouldPayWindowMsg
               , Would
                  batchId
                  ( [PayCosts acId]
                      <> [ CheckAttackOfOpportunity iid False
                         | not modifiersPreventAttackOfOpportunity
                            && ( DoesNotProvokeAttacksOfOpportunity
                                  `notElem` cdAttackOfOpportunityModifiers cardDef
                               )
                            && isNothing (cdFastWindow cardDef)
                            && all (`notElem` nonAttackOfOpportunityActions) actions
                            && totalActions
                            > 0
                         ]
                      <> [PayCostFinished acId]
                  )
               ]
          pure c
        ForAbility a@Ability {..} -> do
          modifiers' <- getModifiers iid
          let
            modifiersPreventAttackOfOpportunity =
              any
                ((`elem` modifiers') . ActionDoesNotCauseAttacksOfOpportunity)
                (abilityActions a)
          push $ PayCostFinished acId
          startAbilityPayment
            c
            iid
            (mkWindow Timing.When Window.NonFast) -- TODO: a thing
            abilityType
            abilitySource
            ( abilityDoesNotProvokeAttacksOfOpportunity
                || modifiersPreventAttackOfOpportunity
            )
          pure c
    PayCost acId iid skipAdditionalCosts cost | acId == activeCostId c -> do
      let
        source = activeCostSource c
        actions = case activeCostActions c of
          [] -> error "action expected"
          as -> as

      canStillAfford <- getCanAffordCost iid source actions (activeCostWindows c) cost
      if canStillAfford
        then payCost msg c iid skipAdditionalCosts cost
        else pure c
    SetCost acId cost | acId == activeCostId c -> do
      pure $ c {activeCostCosts = cost}
    PaidCost acId _ _ payment | acId == activeCostId c -> do
      pure $ c & costPaymentsL <>~ payment
    PayCostFinished acId | acId == activeCostId c -> do
      case activeCostTarget c of
        ForAbility ability -> do
          let
            isAction = isActionAbility ability
            actions = Action.Activate : abilityActions ability
            iid = activeCostInvestigator c
          whenActivateAbilityWindow <-
            checkWindows
              [mkWhen (Window.ActivateAbility iid (activeCostWindows c) ability)]
          afterActivateAbilityWindow <-
            checkWindows
              [mkAfter (Window.ActivateAbility iid (activeCostWindows c) ability)]
          afterMsgs <-
            if isAction
              then do
                afterWindowMsgs <-
                  checkWindows [mkAfter (Window.PerformAction iid action) | action <- actions]
                pure [afterWindowMsgs, FinishAction]
              else pure []
          -- TODO: this will not work for ForcedWhen, but this currently only applies to IntelReport
          isForced <- isForcedAbility iid ability
          pushAll
            $ [whenActivateAbilityWindow | not isForced]
            <> [ UseCardAbility
                  iid
                  (abilitySource ability)
                  (abilityIndex ability)
                  (activeCostWindows c)
                  (activeCostPayments c)
               ]
            <> afterMsgs
            <> [afterActivateAbilityWindow | not isForced]
        ForCard isPlayAction card -> do
          let iid = activeCostInvestigator c
          let actions = [Action.Play | isPlayAction == IsPlayAction] <> cdActions (toCardDef card)
          pushAll
            $ [ PlayCard iid card Nothing (activeCostWindows c) False
              , PaidForCardCost iid card (activeCostPayments c)
              ]
            <> [SealedChaosToken token card | token <- activeCostSealedChaosTokens c]
            <> [FinishAction | notNull actions]
            <> [TakenActions iid actions | notNull actions]
        ForCost card -> pushAll [SealedChaosToken token card | token <- activeCostSealedChaosTokens c]
      push PaidAllCosts
      pure c
    _ -> pure c

targetToCard :: HasGame m => Target -> m Card
targetToCard = \case
  AssetTarget aid -> field AssetCard aid
  EventTarget aid -> field EventCard aid
  SkillTarget aid -> field SkillCard aid
  EnemyTarget aid -> field EnemyCard aid
  TreacheryTarget aid -> field TreacheryCard aid
  LocationTarget aid -> field LocationCard aid
  CardTarget c -> pure c
  SearchedCardTarget cId -> getCard cId
  CardIdTarget cId -> getCard cId
  unknown -> error $ "unhandled: " <> show unknown

sourceToCard :: HasGame m => Source -> m Card
sourceToCard = targetToCard . sourceToTarget
