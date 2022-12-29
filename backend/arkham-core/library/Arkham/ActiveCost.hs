{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.ActiveCost
  ( module Arkham.ActiveCost
  , module X
  ) where

import Arkham.Prelude

import Arkham.ActiveCost.Base as X

import Arkham.Ability
import Arkham.Action hiding ( Ability, TakenAction )
import Arkham.Action qualified as Action
import Arkham.Asset.Types
  ( Field (AssetCard, AssetController, AssetSealedTokens, AssetUses) )
import Arkham.Asset.Uses ( useTypeCount )
import Arkham.Card
import Arkham.Card.Cost
import Arkham.ChaosBag.Base
import Arkham.Classes
import Arkham.Cost hiding ( PaidCost )
import Arkham.Cost.FieldCost
import Arkham.Deck qualified as Deck
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Id
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Matcher hiding ( AssetCard, PlayCard )
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

activeCostActions :: ActiveCost -> [Action]
activeCostActions ac = case activeCostTarget ac of
  ForAbility a -> [fromMaybe Action.Ability (abilityAction a)]
  ForCard isPlayAction c -> if null (cdActions $ toCardDef c)
    then [ Action.Play | isPlayAction == IsPlayAction ]
    else cdActions $ toCardDef c
  ForCost _ -> []

addActiveCostCost :: Cost -> ActiveCost -> ActiveCost
addActiveCostCost cost ac = ac & costsL <>~ cost

activeCostSource :: ActiveCost -> Source
activeCostSource ac = case activeCostTarget ac of
  ForAbility a -> abilitySource a
  ForCard _ c -> CardIdSource $ toCardId c
  ForCost c -> CardIdSource $ toCardId c

costsL :: Lens' ActiveCost Cost
costsL = lens activeCostCosts $ \m x -> m { activeCostCosts = x }

costPaymentsL :: Lens' ActiveCost Payment
costPaymentsL = lens activeCostPayments $ \m x -> m { activeCostPayments = x }

costSealedTokensL :: Lens' ActiveCost [Token]
costSealedTokensL =
  lens activeCostSealedTokens $ \m x -> m { activeCostSealedTokens = x }

activeCostPaid :: ActiveCost -> Bool
activeCostPaid = (== Free) . activeCostCosts

matchTarget :: [Action] -> ActionTarget -> Action -> Bool
matchTarget takenActions (FirstOneOf as) action =
  action `elem` as && all (`notElem` takenActions) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCostModifier :: HasGame m => ActiveCost -> m Int
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
      ForCard{} -> n
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
startAbilityPayment activeCost@ActiveCost { activeCostId } iid window abilityType abilitySource abilityDoesNotProvokeAttacksOfOpportunity
  = case abilityType of
    Objective aType -> startAbilityPayment
      activeCost
      iid
      window
      aType
      abilitySource
      abilityDoesNotProvokeAttacksOfOpportunity
    ForcedAbility _ -> pure ()
    SilentForcedAbility _ -> pure ()
    ForcedAbilityWithCost _ cost -> push (PayCost activeCostId iid False cost)
    AbilityEffect cost -> push (PayCost activeCostId iid False cost)
    FastAbility cost -> push (PayCost activeCostId iid False cost)
    ReactionAbility _ cost -> push (PayCost activeCostId iid False cost)
    ActionAbilityWithBefore mAction _ cost -> do
      -- we do not know which ability will be chosen
      -- for now we assume this will trigger attacks of opportunity
      -- we also skip additional cost checks and abilities of this type
      -- will need to trigger the appropriate check
      pushAll
        (PayCost activeCostId iid True cost
        : [ TakenAction iid action | action <- maybeToList mAction ]
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
        then pushAll
          (PayCost activeCostId iid False cost
          : [ TakenAction iid action | action <- maybeToList mAction ]
          <> [ CheckAttackOfOpportunity iid False
             | not abilityDoesNotProvokeAttacksOfOpportunity
             ]
          )
        else pushAll
          (PayCost activeCostId iid False cost
          : [ TakenAction iid action | action <- maybeToList mAction ]
          )
    ActionAbility mAction cost -> do
      let action = fromMaybe Action.Ability $ mAction
      beforeWindowMsg <- checkWindows
        [Window Timing.When (Window.PerformAction iid action)]
      if action
        `notElem` [Action.Fight, Action.Evade, Action.Resign, Action.Parley]
      then
        pushAll
          ([ BeginAction
           , beforeWindowMsg
           , PayCost activeCostId iid False cost
           , TakenAction iid action
           ]
          <> [ CheckAttackOfOpportunity iid False
             | not abilityDoesNotProvokeAttacksOfOpportunity
             ]
          )
      else
        pushAll
          $ [ BeginAction
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
              [] -> [ Action.Play | isPlayAction == IsPlayAction ]
              as -> as
          beforeWindowMsg <- checkWindows
            $ map (Window Timing.When . Window.PerformAction iid) actions
          pushAll
            $ [ BeginAction
              , beforeWindowMsg
              , PayCost acId iid False (activeCostCosts c)
              ]
            <> map (TakenAction iid) actions
            <> [ CheckAttackOfOpportunity iid False
               | not modifiersPreventAttackOfOpportunity
                 && (DoesNotProvokeAttacksOfOpportunity
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
            modifiersPreventAttackOfOpportunity = maybe
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
            (abilityDoesNotProvokeAttacksOfOpportunity
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
        OrCost xs -> do
          push $ chooseOne iid $ map
            (\x -> Label
              (displayCostType x)
              [PayCost acId iid skipAdditionalCosts x]
            )
            xs
          pure c
        Costs xs ->
          c <$ pushAll [ PayCost acId iid skipAdditionalCosts x | x <- xs ]
        UpTo 0 _ -> pure c
        UpTo n cost' -> do
          canAfford <- andM $ map
            (\a -> getCanAffordCost iid source (Just a) [] cost')
            actions
          maxUpTo <- case cost' of
            ResourceCost resources -> fieldMap
              InvestigatorResources
              (\x -> min n (x `div` resources))
              iid
            _ -> pure n
          c <$ when
            canAfford
            (push $ Ask iid $ ChoosePaymentAmounts
              ("Pay " <> displayCostType cost)
              Nothing
              [ PaymentAmountChoice iid 0 maxUpTo
                  $ PayCost acId iid skipAdditionalCosts cost'
              ]
            )
        DiscardTopOfDeckCost n -> do
          cards <- fieldMap
            InvestigatorDeck
            (map PlayerCard . take n . unDeck)
            iid
          push $ DiscardTopOfDeck iid n Nothing
          withPayment $ DiscardCardPayment cards
        ExhaustCost target -> do
          push (Exhaust target)
          withPayment $ ExhaustPayment [target]
        ExhaustAssetCost matcher -> do
          targets <- map AssetTarget <$> selectList (matcher <> AssetReady)
          c <$ push
            (chooseOne
              iid
              [ TargetLabel
                  target
                  [PayCost acId iid skipAdditionalCosts (ExhaustCost target)]
              | target <- targets
              ]
            )
        SealCost matcher -> do
          targets <-
            filterM (\t -> matchToken iid t matcher)
              =<< scenarioFieldMap ScenarioChaosBag chaosBagTokens
          pushAll
            [ FocusTokens targets
            , chooseOne
              iid
              [ TargetLabel
                  (TokenTarget target)
                  [PayCost acId iid skipAdditionalCosts (SealTokenCost target)]
              | target <- targets
              ]
            , UnfocusTokens
            ]
          pure c
        SealTokenCost token -> do
          push $ SealToken token
          pure
            $ c
            & costPaymentsL
            <>~ SealTokenPayment token
            & costSealedTokensL
            %~ (token :)
        ReleaseTokensCost n -> do
          case source of
            AssetSource aid -> do
              tokens <- field AssetSealedTokens aid
              pushAll
                $ [ FocusTokens tokens
                  , chooseN
                    iid
                    n
                    [ TargetLabel
                        (TokenTarget t)
                        [ PayCost
                            acId
                            iid
                            skipAdditionalCosts
                            (ReleaseTokenCost t)
                        ]
                    | t <- tokens
                    ]
                  , UnfocusTokens
                  ]
            _ -> error "Unhandled source for releasing tokens cost"
          pure c
        ReleaseTokenCost t -> do
          push $ UnsealToken t
          pure $ c & (costPaymentsL <>~ ReleaseTokenPayment t)
        SupplyCost matcher supply -> do
          iid' <-
            selectJust $ InvestigatorWithSupply supply <> InvestigatorAt matcher
          push $ UseSupply iid' supply
          withPayment $ SupplyPayment supply
        DiscardCost target -> do
          pushAll [DiscardedCost target, Discard target]
          withPayment $ DiscardPayment [target]
        DiscardCardCost card -> do
          push (DiscardCard iid (toCardId card))
          withPayment $ DiscardCardPayment [card]
        DiscardDrawnCardCost -> do
          let
            getDrawnCard [] = error "can not find drawn card in windows"
            getDrawnCard (x : xs) = case x of
              Window _ (Window.DrawCard _ card' _) -> card'
              _ -> getDrawnCard xs
            card = getDrawnCard (activeCostWindows c)
          push (DiscardCard iid (toCardId card))
          withPayment $ DiscardCardPayment [card]
        ExileCost target -> do
          push (Exile target)
          withPayment $ ExilePayment [target]
        RemoveCost target -> do
          push (RemoveFromGame target)
          withPayment $ RemovePayment [target]
        EnemyDoomCost x matcher -> do
          enemies <- selectListMap EnemyTarget matcher
          push $ chooseOrRunOne
            iid
            [ TargetLabel target [PlaceDoom target x] | target <- enemies ]
          withPayment $ DoomPayment x
        DoomCost _ (AgendaMatcherTarget matcher) x -> do
          agendas <- selectListMap AgendaTarget matcher
          pushAll [ PlaceDoom target x | target <- agendas ]
          withPayment $ DoomPayment (x * length agendas)
        DoomCost _ target x -> do
          push (PlaceDoom target x)
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
            push $ chooseOrRunOne
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
            ForAbility{} -> push $ SpendResources iid x
            ForCost{} -> push $ SpendResources iid x
            ForCard _ card -> do
              iids <- filter (/= iid) <$> getInvestigatorIds
              iidsWithModifiers <- for iids $ \iid' -> do
                modifiers <- getModifiers (InvestigatorTarget iid')
                pure (iid', modifiers)
              canHelpPay <- flip filterM iidsWithModifiers $ \(_, modifiers) ->
                do
                  flip anyM modifiers $ \case
                    CanSpendResourcesOnCardFromInvestigator iMatcher cMatcher
                      -> liftA2
                        (&&)
                        (member iid <$> select iMatcher)
                        (pure $ cardMatch card cMatcher)
                    _ -> pure False
              if null canHelpPay
                then push (SpendResources iid x)
                else do
                  iidsWithResources <- forToSnd
                    (iid : map fst canHelpPay)
                    (field InvestigatorResources)
                  push
                    (Ask iid
                    $ ChoosePaymentAmounts
                        ("Pay " <> tshow x <> " resources")
                        (Just x)
                    $ map
                        (\(iid', resources) -> PaymentAmountChoice
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
          c <$ if actionRemainingCount == 0
            then pure ()
            else push
              (chooseOne
                iid
                [ Label
                  "Spend 1 additional action"
                  [ PayCost acId iid skipAdditionalCosts (ActionCost 1)
                  , PaidAbilityCost iid Nothing AdditionalActionPayment
                  , msg
                  ]
                , Label
                  ("Done spending additional actions ("
                  <> tshow currentlyPaid
                  <> " spent so far)"
                  )
                  []
                ]
              )
        ActionCost x -> do
          costModifier <- if skipAdditionalCosts
            then pure 0
            else getActionCostModifier c
          let
            modifiedActionCost = max 0 (x + costModifier)
            mAction = case activeCostTarget c of
              ForAbility a -> abilityAction a
              _ -> Nothing
          push (SpendActions iid source mAction modifiedActionCost)
          withPayment $ ActionPayment x
        UseCost assetMatcher uType n -> do
          assets <- selectList assetMatcher
          push $ chooseOrRunOne
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
            push $ chooseOrRunOne
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

          push $ Ask iid $ ChoosePaymentAmounts
            ("Pay " <> displayCostType cost)
            Nothing
            [ PaymentAmountChoice iid n maxUses
                $ PayCost
                    acId
                    iid
                    skipAdditionalCosts
                    (UseCost assetMatcher uType 1)
            ]
          pure c
        ClueCost x -> do
          push $ InvestigatorSpendClues iid x
          withPayment $ CluePayment x
        PerPlayerClueCost x -> do
          totalClues <- getPlayerCountValue (PerPlayer x)
          push $ InvestigatorSpendClues iid totalClues
          withPayment $ CluePayment totalClues
        PlaceClueOnLocationCost x -> do
          push $ InvestigatorPlaceCluesOnLocation iid x
          withPayment $ CluePayment x
        GroupClueCost x locationMatcher -> do
          totalClues <- getPlayerCountValue x
          iids <-
            selectList
            $ InvestigatorAt locationMatcher
            <> InvestigatorWithAnyClues
          iidsWithClues <- filter ((> 0) . snd)
            <$> forToSnd iids (getSpendableClueCount . pure)
          case iidsWithClues of
            [(iid', _)] ->
              c <$ push (PayCost acId iid' True (ClueCost totalClues))
            _ -> do
              let
                paymentOptions = map
                  (\(iid', clues) -> PaymentAmountChoice
                    iid'
                    0
                    clues
                    (PayCost acId iid' True (ClueCost 1))
                  )
                  iidsWithClues
              leadInvestigatorId <- getLeadInvestigatorId
              c <$ push
                (Ask leadInvestigatorId $ ChoosePaymentAmounts
                  (displayCostType cost)
                  (Just totalClues)
                  paymentOptions
                )
          -- push (SpendClues totalClues iids)
          -- withPayment $ CluePayment totalClues
        HandDiscardCost x cardMatcher -> do
          handCards <- fieldMap
            InvestigatorHand
            (mapMaybe (preview _PlayerCard))
            iid
          let
            notCostCard = case activeCostTarget c of
              ForAbility{} -> const True
              ForCard _ card' -> (/= card')
              ForCost card' -> (/= card')
            cards = filter
              (and . sequence
                [(`cardMatch` cardMatcher), notCostCard . PlayerCard]
              )
              handCards
          push $ chooseN
            iid
            x
            [ TargetLabel
                (CardIdTarget $ toCardId card)
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
          push $ chooseOne
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
          controller <- fieldMap
            AssetController
            (fromJustNote "Missing controller")
            assetId
          push $ ReturnToHand controller $ AssetTarget assetId
          withPayment $ ReturnToHandPayment card
        DiscardHandCost -> do
          handCards <- fieldMap
            InvestigatorHand
            (mapMaybe (preview _PlayerCard))
            iid
          push $ DiscardHand iid
          withPayment $ DiscardCardPayment $ map PlayerCard handCards
        DiscardFromCost x zone cardMatcher -> do
          let
            getCards = \case
              FromHandOf whoMatcher ->
                selectList (InHandOf whoMatcher <> BasicCardMatch cardMatcher)
              FromPlayAreaOf whoMatcher -> do
                assets <- selectList $ AssetControlledBy whoMatcher
                traverse (field AssetCard) assets
              CostZones zs -> concatMapM getCards zs
          cards <- getCards zone
          c <$ push
            (chooseN
              iid
              x
              [ TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ PayCost
                      acId
                      iid
                      skipAdditionalCosts
                      (DiscardCost $ CardIdTarget $ toCardId card)
                  ]
              | card <- cards
              ]
            )
        SkillIconCost x skillTypes -> do
          handCards <- fieldMap
            InvestigatorHand
            (mapMaybe (preview _PlayerCard))
            iid
          let
            cards = filter ((> 0) . fst) $ map
              (toFst
                (count (`member` insertSet WildIcon skillTypes)
                . cdSkills
                . toCardDef
                )
              )
              handCards
            cardMsgs = map
              (\(n, card) -> if n >= x
                then TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ DiscardCard iid (toCardId card)
                  , PaidAbilityCost
                    iid
                    Nothing
                    (SkillIconPayment $ cdSkills $ toCardDef card)
                  ]
                else TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ DiscardCard iid (toCardId card)
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
          handCards <- fieldMap
            InvestigatorHand
            (mapMaybe (preview _PlayerCard) . filter (`cardMatch` NonWeakness))
            iid
          let
            cards =
              map (toFst (maybe 0 toPrintedCost . cdCost . toCardDef)) handCards
            cardMsgs = map
              (\(n, card) -> if n >= x
                then TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ DiscardCard iid (toCardId card)
                  , PaidAbilityCost iid Nothing
                    $ DiscardCardPayment [PlayerCard card]
                  ]
                else TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ DiscardCard iid (toCardId card)
                  , PaidAbilityCost iid Nothing
                    $ DiscardCardPayment [PlayerCard card]
                  , PayCost acId iid skipAdditionalCosts
                    $ DiscardCombinedCost (x - n)
                  ]
              )
              cards
          c <$ push (chooseOne iid cardMsgs)
        ShuffleDiscardCost 0 _ -> pure c
        ShuffleDiscardCost n cardMatcher -> do
          cards <- fieldMap
            InvestigatorDiscard
            (map PlayerCard . filter (`cardMatch` cardMatcher))
            iid
          let
            cardMsgs = map
              (\card -> TargetLabel
                (CardIdTarget $ toCardId card)
                [ RemoveFromDiscard iid (toCardId card)
                , ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [card]
                , PaidAbilityCost iid Nothing $ CardPayment card
                , PayCost acId iid skipAdditionalCosts
                  $ ShuffleDiscardCost (n - 1) cardMatcher
                ]
              )
              cards
          c <$ pushAll [FocusCards cards, chooseOne iid cardMsgs, UnfocusCards]
        Free -> pure c
    PaidCost acId _ _ payment | acId == activeCostId c ->
      pure $ c & costPaymentsL <>~ payment
    PayCostFinished acId | acId == activeCostId c -> do
      case activeCostTarget c of
        ForAbility ability -> do
          let
            isAction = isActionAbility ability
            action = fromMaybe Action.Ability (abilityAction ability)
            iid = activeCostInvestigator c
          whenActivateAbilityWindow <- checkWindows
            [Window Timing.When (Window.ActivateAbility iid ability)]
          afterMsgs <- if isAction
            then do
              afterWindowMsgs <- checkWindows
                [Window Timing.After (Window.PerformAction iid action)]
              pure [afterWindowMsgs, FinishAction]
            else pure []
          pushAll
            $ [ whenActivateAbilityWindow | not (isForcedAbility ability) ]
            <> [ UseCardAbility
                   iid
                   (abilitySource ability)
                   (abilityIndex ability)
                   (activeCostWindows c)
                   (activeCostPayments c)
               ]
            <> afterMsgs
        ForCard isPlayAction card -> do
          let iid = activeCostInvestigator c
          pushAll
            $ [ PlayCard iid card Nothing (activeCostWindows c) False
              , PaidForCardCost iid card (activeCostPayments c)
              ]
            <> [ SealedToken token card | token <- activeCostSealedTokens c ]
            <> [ FinishAction | isPlayAction == IsPlayAction ]
        ForCost card -> pushAll
          [ SealedToken token card | token <- activeCostSealedTokens c ]
      pure c
    _ -> pure c
