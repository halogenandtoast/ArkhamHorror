{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.ActiveCost (
  module Arkham.ActiveCost,
  module X,
) where

import Arkham.Prelude

import Arkham.ActiveCost.Base as X

import Arkham.Ability hiding (PaidCost)
import Arkham.Action hiding (Ability, TakenAction)
import Arkham.Action qualified as Action
import Arkham.Asset.Types (
  Field (AssetCard, AssetController, AssetSealedChaosTokens, AssetUses),
 )
import Arkham.Asset.Uses (useTypeCount)
import Arkham.Card
import Arkham.Card.Cost
import Arkham.ChaosBag.Base
import Arkham.ChaosToken
import Arkham.Classes
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
import Arkham.Helpers.Message.Discard
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
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types (Field (..))
import Arkham.Skill.Types (Field (..))
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Treachery.Types (Field (..))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

activeCostActions :: ActiveCost -> [Action]
activeCostActions ac = case activeCostTarget ac of
  ForAbility a -> [fromMaybe Action.Ability (abilityAction a)]
  ForCard isPlayAction c ->
    if null (cdActions $ toCardDef c)
      then [Action.Play | isPlayAction == IsPlayAction]
      else cdActions $ toCardDef c
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

matchTarget :: [Action] -> ActionTarget -> Action -> Bool
matchTarget takenActions (FirstOneOf as) action =
  action `elem` as && all (`notElem` takenActions) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCostModifier :: (HasGame m) => ActiveCost -> m Int
getActionCostModifier ac = do
  let iid = activeCostInvestigator ac
  takenActions <- field InvestigatorActionsTaken iid
  modifiers <- getModifiers (InvestigatorTarget iid)
  pure $ foldr (applyModifier takenActions) 0 modifiers
 where
  actions = case activeCostActions ac of
    [] -> error "expected action"
    as -> as
  applyModifier takenActions (ActionCostOf match m) n =
    -- For cards we've already calculated the cost as an additional cost for
    -- the action specifically
    case activeCostTarget ac of
      ForCard {} -> n
      _ -> if any (matchTarget takenActions match) actions then n + m else n
  applyModifier _ _ n = n

countAdditionalActionPayments :: Payment -> Int
countAdditionalActionPayments AdditionalActionPayment = 1
countAdditionalActionPayments (Payments ps) =
  sum $ map countAdditionalActionPayments ps
countAdditionalActionPayments _ = 0

startAbilityPayment
  :: ActiveCost
  -> InvestigatorId
  -> Window
  -> AbilityType
  -> Source
  -> Bool
  -> GameT ()
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
    ForcedAbilityWithCost _ cost -> push (PayCost activeCostId iid False cost)
    AbilityEffect cost -> push (PayCost activeCostId iid False cost)
    FastAbility cost -> push (PayCost activeCostId iid False cost)
    ForcedWhen _ aType ->
      startAbilityPayment
        activeCost
        iid
        window
        aType
        abilitySource
        abilityDoesNotProvokeAttacksOfOpportunity
    ReactionAbility _ cost -> push (PayCost activeCostId iid False cost)
    ActionAbilityWithBefore mAction _ cost -> do
      -- we do not know which ability will be chosen
      -- for now we assume this will trigger attacks of opportunity
      -- we also skip additional cost checks and abilities of this type
      -- will need to trigger the appropriate check
      pushAll
        ( PayCost activeCostId iid True cost
            : [TakenAction iid action | action <- maybeToList mAction]
              <> [ CheckAttackOfOpportunity iid False
                 | not abilityDoesNotProvokeAttacksOfOpportunity
                 ]
        )
    ActionAbilityWithSkill mAction _ cost ->
      if mAction
        `notElem` [ Just Action.Fight
                  , Just Action.Evade
                  , Just Action.Resign
                  , Just Action.Parley
                  ]
        then
          pushAll
            ( PayCost activeCostId iid False cost
                : [TakenAction iid action | action <- maybeToList mAction]
                  <> [ CheckAttackOfOpportunity iid False
                     | not abilityDoesNotProvokeAttacksOfOpportunity
                     ]
            )
        else
          pushAll
            ( PayCost activeCostId iid False cost
                : [TakenAction iid action | action <- maybeToList mAction]
            )
    ActionAbility mAction cost -> do
      let action = fromMaybe Action.Ability $ mAction
      beforeWindowMsg <-
        checkWindows
          [Window Timing.When (Window.PerformAction iid action)]
      if action
        `notElem` [Action.Fight, Action.Evade, Action.Resign, Action.Parley]
        then
          pushAll
            ( [ BeginAction
              , beforeWindowMsg
              , PayCost activeCostId iid False cost
              , TakenAction iid action
              ]
                <> [ CheckAttackOfOpportunity iid False
                   | not abilityDoesNotProvokeAttacksOfOpportunity
                   ]
            )
        else
          pushAll $
            [ BeginAction
            , beforeWindowMsg
            , PayCost activeCostId iid False cost
            , TakenAction iid action
            ]

nonAttackOfOpportunityActions :: [Action]
nonAttackOfOpportunityActions =
  [Action.Fight, Action.Evade, Action.Resign, Action.Parley]

instance RunMessage ActiveCost where
  runMessage msg c = case msg of
    CreatedCost acId | acId == activeCostId c -> do
      let iid = activeCostInvestigator c
      case activeCostTarget c of
        ForCost _ -> do
          pushAll
            [PayCost acId iid False (activeCostCosts c), PayCostFinished acId]
          pure c
        ForCard isPlayAction card -> do
          modifiers' <- getModifiers (InvestigatorTarget iid)
          let
            cardDef = toCardDef card
            modifiersPreventAttackOfOpportunity =
              ActionDoesNotCauseAttacksOfOpportunity Action.Play
                `elem` modifiers'
            actions = case cdActions cardDef of
              [] -> [Action.Play | isPlayAction == IsPlayAction]
              as -> as
          beforeWindowMsg <-
            checkWindows $
              map (Window Timing.When . Window.PerformAction iid) actions
          pushAll $
            [ BeginAction
            , beforeWindowMsg
            , PayCost acId iid False (activeCostCosts c)
            ]
              <> map (TakenAction iid) actions
              <> [ CheckAttackOfOpportunity iid False
                 | not modifiersPreventAttackOfOpportunity
                    && ( DoesNotProvokeAttacksOfOpportunity
                          `notElem` (cdAttackOfOpportunityModifiers cardDef)
                       )
                    && (isNothing $ cdFastWindow cardDef)
                    && (any (`notElem` nonAttackOfOpportunityActions) actions)
                 ]
              <> [PayCostFinished acId]
          pure c
        ForAbility a@Ability {..} -> do
          modifiers' <- getModifiers (InvestigatorTarget iid)
          let
            modifiersPreventAttackOfOpportunity =
              maybe
                False
                ((`elem` modifiers') . ActionDoesNotCauseAttacksOfOpportunity)
                (abilityAction a)
          push $ PayCostFinished acId
          startAbilityPayment
            c
            iid
            (Window Timing.When Window.NonFast) -- TODO: a thing
            abilityType
            abilitySource
            ( abilityDoesNotProvokeAttacksOfOpportunity
                || modifiersPreventAttackOfOpportunity
            )
          pure c
    PayCost acId iid skipAdditionalCosts cost | acId == activeCostId c -> do
      let
        withPayment payment = pure $ c & costPaymentsL <>~ payment
        source = activeCostSource c
        actions = case activeCostActions c of
          [] -> error "action expected"
          as -> as
      case cost of
        ResolveEachHauntedAbility lid -> do
          hauntedAbilities <-
            selectList $
              HauntedAbility
                <> AbilityOnLocation
                  (LocationWithId lid)
          when (notNull hauntedAbilities) $
            push $
              chooseOneAtATime
                iid
                [AbilityLabel iid ab [] [] | ab <- hauntedAbilities]
          -- No need to record payment... yet
          pure c
        OrCost xs -> do
          push $
            chooseOne iid $
              map
                ( \x ->
                    Label
                      (displayCostType x)
                      [PayCost acId iid skipAdditionalCosts x]
                )
                xs
          pure c
        Costs xs ->
          c <$ pushAll [PayCost acId iid skipAdditionalCosts x | x <- xs]
        UpTo 0 _ -> pure c
        UpTo n cost' -> do
          canAfford <-
            andM $
              map
                (\a -> getCanAffordCost iid source (Just a) [] cost')
                actions
          maxUpTo <- case cost' of
            ResourceCost resources -> do
              availableResources <- getSpendableResources iid
              pure $ min n (availableResources `div` resources)
            _ -> pure n
          when canAfford $
            push $
              Ask iid $
                ChoosePaymentAmounts
                  ("Pay " <> displayCostType cost)
                  Nothing
                  [ PaymentAmountChoice iid 0 maxUpTo $
                      PayCost acId iid skipAdditionalCosts cost'
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
          push $
            CreateWindowModifierEffect
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
                  iid
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
                iid
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
          pure $
            c
              & costPaymentsL
                <>~ SealChaosTokenPayment token
              & costSealedChaosTokensL
                %~ (token :)
        ReleaseChaosTokensCost n -> do
          case source of
            AssetSource aid -> do
              tokens <- field AssetSealedChaosTokens aid
              pushAll $
                [ FocusChaosTokens tokens
                , chooseN
                    iid
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
          pushAll [DiscardedCost target, Discard (activeCostSource c) target]
          withPayment $ DiscardPayment [(zone, card)]
        DiscardAssetCost matcher -> do
          targets <- map AssetTarget <$> selectList (matcher <> AssetReady)
          push
            ( chooseOne
                iid
                [ TargetLabel
                  target
                  [PayCost acId iid skipAdditionalCosts (DiscardCost FromPlay target)]
                | target <- targets
                ]
            )
          pure c
        DiscardCardCost card -> do
          push $ toMessage $ discardCard iid (activeCostSource c) card
          withPayment $ DiscardCardPayment [card]
        DiscardDrawnCardCost -> do
          let
            getDrawnCard [] = error "can not find drawn card in windows"
            getDrawnCard (x : xs) = case x of
              Window _ (Window.DrawCard _ card' _) -> card'
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
        EnemyDoomCost x matcher -> do
          enemies <- selectListMap EnemyTarget matcher
          push $
            chooseOrRunOne
              iid
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
            pushAll [AssetDamage aid source 0 x, CheckDefeated source]
            withPayment $ HorrorPayment x
          _ -> error "can't target for horror cost"
        HorrorCostX source' -> do
          -- see: The Black Book as that is the only card that uses this
          let
            getPlayedCard [] = error "can not find played card in windows"
            getPlayedCard (x : xs) = case x of
              Window _ (Window.PlayCard _ card') -> card'
              _ -> getPlayedCard xs
            card = getPlayedCard (activeCostWindows c)
          availableResources <- getSpendableResources iid
          requiredResources <- getModifiedCardCost iid card
          let minimumHorror = max 1 (requiredResources - availableResources)
          sanity <- field InvestigatorRemainingSanity iid
          push $
            Ask iid $
              ChoosePaymentAmounts
                "Pay X Horror"
                Nothing
                [ PaymentAmountChoice iid minimumHorror sanity $
                    PayCost acId iid skipAdditionalCosts (HorrorCost source' (InvestigatorTarget iid) 1)
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
            pushAll [AssetDamage aid source x 0, CheckDefeated source]
            withPayment $ DamagePayment x
          _ -> error "can't target for damage cost"
        DirectDamageCost _ investigatorMatcher x -> do
          investigators <- selectList investigatorMatcher
          case investigators of
            [iid'] -> do
              push $ InvestigatorDirectDamage iid' source x 0
              withPayment $ DirectDamagePayment x
            _ -> error "exactly one investigator expected for direct damage"
        InvestigatorDamageCost source' investigatorMatcher damageStrategy x ->
          do
            investigators <- selectList investigatorMatcher
            push $
              chooseOrRunOne
                iid
                [ targetLabel
                  iid'
                  [InvestigatorAssignDamage iid' source' damageStrategy x 0]
                | iid' <- investigators
                ]
            withPayment $ InvestigatorDamagePayment x
        FieldResourceCost (FieldCost mtchr fld) -> do
          n <- getSum <$> selectAgg Sum fld mtchr
          push $ PayCost acId iid skipAdditionalCosts (ResourceCost n)
          pure c
        ResourceCost x -> do
          case activeCostTarget c of
            ForAbility {} -> push $ SpendResources iid x
            ForCost {} -> push $ SpendResources iid x
            ForCard _ card -> do
              iids <- filter (/= iid) <$> getInvestigatorIds
              iidsWithModifiers <- for iids $ \iid' -> do
                modifiers <- getModifiers (InvestigatorTarget iid')
                pure (iid', modifiers)
              canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) ->
                do
                  flip anyM modifiers $ \case
                    CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher ->
                      liftA2
                        (&&)
                        (member iid <$> select iMatcher)
                        (pure $ cardMatch card cMatcher)
                    _ -> pure False
              if null canHelpPay
                then push (SpendResources iid x)
                else do
                  iidsWithResources <-
                    forToSnd
                      (iid : map fst canHelpPay)
                      (getSpendableResources)
                  push
                    ( Ask iid
                        $ ChoosePaymentAmounts
                          ("Pay " <> tshow x <> " resources")
                          (Just x)
                        $ map
                          ( \(iid', resources) ->
                              PaymentAmountChoice
                                iid'
                                0
                                resources
                                (SpendResources iid' 1)
                          )
                          iidsWithResources
                    )
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
                      iid
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
          costModifier <-
            if skipAdditionalCosts
              then pure 0
              else getActionCostModifier c
          let
            modifiedActionCost = max 0 (x + costModifier)
            mAction = case activeCostTarget c of
              ForAbility a -> abilityAction a
              _ -> Nothing
            source' = case activeCostTarget c of
              ForAbility a -> AbilitySource (abilitySource a) (abilityIndex a)
              _ -> source
          push (SpendActions iid source' mAction modifiedActionCost)
          withPayment $ ActionPayment x
        UseCost assetMatcher uType n -> do
          assets <- selectList assetMatcher
          push $
            chooseOrRunOne
              iid
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
                Window _ (Window.DrawCards _ cards) -> length cards
                _ -> getDrawnCards xs
              n = getDrawnCards (activeCostWindows c)
            assets <- selectList assetMatcher
            push $
              chooseOrRunOne
                iid
                [ TargetLabel
                  (AssetTarget aid)
                  [SpendUses (AssetTarget aid) uType n]
                | aid <- assets
                ]
            withPayment $ UsesPayment n
        UseCostUpTo assetMatcher uType n m -> do
          assets <- selectList assetMatcher
          uses <-
            sum <$> traverse (fieldMap AssetUses (useTypeCount uType)) assets
          let maxUses = min uses m

          push $
            Ask iid $
              ChoosePaymentAmounts
                ("Pay " <> displayCostType cost)
                Nothing
                [ PaymentAmountChoice iid n maxUses $
                    PayCost
                      acId
                      iid
                      skipAdditionalCosts
                      (UseCost assetMatcher uType 1)
                ]
          pure c
        ClueCost gv -> do
          totalClues <- getPlayerCountValue gv
          push $ InvestigatorSpendClues iid totalClues
          withPayment $ CluePayment iid totalClues
        PlaceClueOnLocationCost x -> do
          push $ InvestigatorPlaceCluesOnLocation iid source x
          withPayment $ CluePayment iid x
        GroupClueCostRange (sVal, eVal) locationMatcher -> do
          mVal <- min eVal . getSum <$> selectAgg Sum InvestigatorClues (InvestigatorAt locationMatcher)
          if mVal == sVal
            then push $ PayCost acId iid skipAdditionalCosts (GroupClueCost (Static sVal) locationMatcher)
            else
              push
                $ questionLabel
                  "Spend 1-3 clues, as a group"
                  iid
                $ DropDown
                  [ (tshow n, PayCost acId iid skipAdditionalCosts (GroupClueCost (Static n) locationMatcher))
                  | n <- [sVal .. mVal]
                  ]
          pure c
        GroupClueCost x locationMatcher -> do
          totalClues <- getPlayerCountValue x
          iids <-
            selectList $
              InvestigatorAt locationMatcher
                <> InvestigatorWithAnyClues
          iidsWithClues <-
            filter ((> 0) . snd)
              <$> forToSnd iids (getSpendableClueCount . pure)
          case iidsWithClues of
            [(iid', _)] ->
              c <$ push (PayCost acId iid' True (ClueCost (Static totalClues)))
            _ -> do
              let
                paymentOptions =
                  map
                    ( \(iid', clues) ->
                        PaymentAmountChoice
                          iid'
                          0
                          clues
                          (PayCost acId iid' True (ClueCost (Static 1)))
                    )
                    iidsWithClues
              leadInvestigatorId <- getLeadInvestigatorId
              push $
                Ask leadInvestigatorId $
                  ChoosePaymentAmounts
                    (displayCostType cost)
                    (Just totalClues)
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
          push $
            chooseN
              iid
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
        ReturnMatchingAssetToHandCost assetMatcher -> do
          assets <- selectList assetMatcher
          push $
            chooseOne
              iid
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
                  iid
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
              filter ((> 0) . fst) $
                map
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
          c <$ push (chooseOne iid cardMsgs)
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
                          , PaidAbilityCost iid Nothing $
                              DiscardCardPayment [PlayerCard card]
                          ]
                      else
                        targetLabel
                          (toCardId card)
                          [ toMessage $ discardCard iid (activeCostSource c) card
                          , PaidAbilityCost iid Nothing $
                              DiscardCardPayment [PlayerCard card]
                          , PayCost acId iid skipAdditionalCosts $
                              DiscardCombinedCost (x - n)
                          ]
                )
                cards
          c <$ push (chooseOne iid cardMsgs)
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
                      , PayCost acId iid skipAdditionalCosts $
                          ShuffleDiscardCost (n - 1) cardMatcher
                      ]
                )
                cards
          c <$ pushAll [FocusCards cards, chooseOne iid cardMsgs, UnfocusCards]
        Free -> pure c
    PaidCost acId _ _ payment
      | acId == activeCostId c ->
          pure $ c & costPaymentsL <>~ payment
    PayCostFinished acId | acId == activeCostId c -> do
      case activeCostTarget c of
        ForAbility ability -> do
          let
            isAction = isActionAbility ability
            action = fromMaybe Action.Ability (abilityAction ability)
            iid = activeCostInvestigator c
          whenActivateAbilityWindow <-
            checkWindows
              [Window Timing.When (Window.ActivateAbility iid ability)]
          afterActivateAbilityWindow <-
            checkWindows
              [Window Timing.After (Window.ActivateAbility iid ability)]
          afterMsgs <-
            if isAction
              then do
                afterWindowMsgs <-
                  checkWindows
                    [Window Timing.After (Window.PerformAction iid action)]
                pure [afterWindowMsgs, FinishAction]
              else pure []
          -- TODO: this will not work for ForcedWhen, but this currently only applies to IntelReport
          isForced <- isForcedAbility iid ability
          pushAll $
            [whenActivateAbilityWindow | not isForced]
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
          pushAll $
            [ PlayCard iid card Nothing (activeCostWindows c) False
            , PaidForCardCost iid card (activeCostPayments c)
            ]
              <> [SealedChaosToken token card | token <- activeCostSealedChaosTokens c]
              <> [FinishAction | isPlayAction == IsPlayAction]
        ForCost card ->
          pushAll
            [SealedChaosToken token card | token <- activeCostSealedChaosTokens c]
      pure c
    _ -> pure c

targetToCard :: (HasGame m) => Target -> m Card
targetToCard = \case
  AssetTarget aid -> field AssetCard aid
  EventTarget aid -> field EventCard aid
  SkillTarget aid -> field SkillCard aid
  EnemyTarget aid -> field EnemyCard aid
  TreacheryTarget aid -> field TreacheryCard aid
  LocationTarget aid -> field LocationCard aid
  CardTarget c -> pure c
  unknown -> error $ "unhandled: " <> show unknown
