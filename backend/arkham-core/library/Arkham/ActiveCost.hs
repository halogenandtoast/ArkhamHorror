module Arkham.ActiveCost where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action hiding ( Ability, TakenAction )
import Arkham.Action qualified as Action
import Arkham.Asset.Attrs ( Field (AssetCard) )
import Arkham.Card
import Arkham.ChaosBag.Base
import Arkham.Classes
import Arkham.Cost hiding (PaidCost)
import Arkham.Effect.Runner
import Arkham.EffectId
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Id
import Arkham.Investigator.Attrs ( Field (..) )
import Arkham.Matcher hiding ( AssetCard )
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Attrs ( Field (..) )
import Arkham.SkillType
import Arkham.Source
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

data ActiveCost = ActiveCost
  { activeCostCosts :: Cost
  , activeCostPayments :: Payment
  , activeCostTarget :: ActiveCostTarget
  , activeCostWindows :: [Window]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActiveCostTarget = ForCard Card | ForAbility Ability
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

costPaymentsL :: Lens' ActiveCost Payment
costPaymentsL = lens activeCostPayments $ \m x -> m { activeCostPayments = x }

activeCostPaid :: ActiveCost -> Bool
activeCostPaid = (== Free) . activeCostCosts

matchTarget :: [Action] -> ActionTarget -> Action -> Bool
matchTarget takenActions (FirstOneOf as) action =
  action `elem` as && all (`notElem` takenActions) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCostModifier
  :: (Monad m, HasGame m) => InvestigatorId -> Maybe Action -> m Int
getActionCostModifier _ Nothing = pure 0
getActionCostModifier iid (Just a) = do
  takenActions <- field InvestigatorActionsTaken iid
  modifiers <- getModifiers (InvestigatorSource iid) (InvestigatorTarget iid)
  pure $ foldr (applyModifier takenActions) 0 modifiers
 where
  applyModifier takenActions (ActionCostOf match m) n =
    if matchTarget takenActions match a then n + m else n
  applyModifier _ _ n = n

countAdditionalActionPayments :: Payment -> Int
countAdditionalActionPayments AdditionalActionPayment = 1
countAdditionalActionPayments (Payments ps) =
  sum $ map countAdditionalActionPayments ps
countAdditionalActionPayments _ = 0

startPayment
  :: InvestigatorId -> Window -> AbilityType -> Source -> Bool -> GameT ()
startPayment iid window abilityType abilitySource abilityDoesNotProvokeAttacksOfOpportunity
  = case abilityType of
    Objective aType -> startPayment
      iid
      window
      aType
      abilitySource
      abilityDoesNotProvokeAttacksOfOpportunity
    ForcedAbility _ -> pure ()
    SilentForcedAbility _ -> pure ()
    ForcedAbilityWithCost _ cost ->
      push (PayAbilityCost abilitySource iid Nothing False cost)
    AbilityEffect cost ->
      push (PayAbilityCost abilitySource iid Nothing False cost)
    FastAbility cost ->
      push (PayAbilityCost abilitySource iid Nothing False cost)
    ReactionAbility _ cost ->
      push (PayAbilityCost abilitySource iid Nothing False cost)
    ActionAbilityWithBefore mAction _ cost -> do
      -- we do not know which ability will be chosen
      -- for now we assume this will trigger attacks of opportunity
      -- we also skip additional cost checks and abilities of this type
      -- will need to trigger the appropriate check
      pushAll
        (PayAbilityCost abilitySource iid mAction True cost
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
          (PayAbilityCost abilitySource iid mAction False cost
          : [ TakenAction iid action | action <- maybeToList mAction ]
          <> [ CheckAttackOfOpportunity iid False
             | not abilityDoesNotProvokeAttacksOfOpportunity
             ]
          )
        else pushAll
          (PayAbilityCost abilitySource iid mAction False cost
          : [ TakenAction iid action | action <- maybeToList mAction ]
          )
    ActionAbility mAction cost -> do
      let action = fromMaybe Action.Ability mAction
      beforeWindowMsg <- checkWindows
        [Window Timing.When (Window.PerformAction iid action)]
      if action
        `notElem` [Action.Fight, Action.Evade, Action.Resign, Action.Parley]
      then
        pushAll
          ([ BeginAction
           , beforeWindowMsg
           , PayAbilityCost abilitySource iid mAction False cost
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
            , PayAbilityCost abilitySource iid mAction False cost
            , TakenAction iid action
            ]

instance RunMessage ActiveCost where
  runMessage msg c = case msg of
    CreatedCost iid source -> do
      case activeCostTarget c of
        ForAbility a -> do
          modifiers' <- getModifiers
            (InvestigatorSource iid)
            (InvestigatorTarget iid)
          let
            modifiersPreventAttackOfOpportunity = maybe
              False
              ((`elem` modifiers') . ActionDoesNotCauseAttacksOfOpportunity)
              (abilityAction a)
          push (PayCostFinished source iid)
          c <$ startPayment
            iid
            (Window Timing.When Window.NonFast) -- TODO: a thing
            abilityType
            abilitySource
            (abilityDoesNotProvokeAttacksOfOpportunity
            || modifiersPreventAttackOfOpportunity
            )
    PayCost source iid mAction skipAdditionalCosts cost -> do
      let withPayment payment = pure $ c & costPaymentsL <>~ payment
      case cost of
        Costs xs ->
          c <$ pushAll
            [ PayCost source iid mAction skipAdditionalCosts x | x <- xs ]
        UpTo 0 _ -> pure c
        UpTo n cost' -> do
          canAfford <- getCanAffordCost iid source mAction [] cost'
          c <$ when
            canAfford
            (push $ Ask iid $ ChoosePaymentAmounts
              ("Pay " <> displayCostType cost)
              Nothing
              [ ( iid
                , (0, n)
                , PayCost source iid mAction skipAdditionalCosts cost'
                )
              ]
            )
            --   iid
            --   [ Label
            --     "Pay dynamic cost"
            --     [ PayAbilityCost source iid mAction skipAdditionalCosts cost'
            --     , PayAbilityCost
            --       source
            --       iid
            --       mAction
            --       skipAdditionalCosts
            --       (UpTo (n - 1) cost')
            --     ]
            --   , Label "Done with dynamic cost" []
            --   ]
            -- )
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
                  [ PayCost
                      source
                      iid
                      mAction
                      skipAdditionalCosts
                      (ExhaustCost target)
                  ]
              | target <- targets
              ]
            )
        SealCost matcher -> do
          targets <-
            filterM (\t -> matchToken iid t matcher)
              =<< scenarioFieldMap ScenarioChaosBag chaosBagTokens
          c <$ push
            (chooseOne
              iid
              [ TargetLabel
                  (TokenTarget target)
                  [ PayCost
                      source
                      iid
                      mAction
                      skipAdditionalCosts
                      (SealTokenCost target)
                  ]
              | target <- targets
              ]
            )
        SealTokenCost token -> do
          push $ SealToken token
          withPayment $ SealTokenPayment token
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
              Window _ (Window.DrawCard _ c _) -> c
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
        InvestigatorDamageCost _ investigatorMatcher damageStrategy x -> do
          investigators <- selectList investigatorMatcher
          push $ chooseOne
            iid
            [ targetLabel
                iid'
                [InvestigatorAssignDamage iid' source damageStrategy x 0]
            | iid' <- investigators
            ]
          withPayment $ InvestigatorDamagePayment x
        ResourceCost x -> do
          push (SpendResources iid x)
          withPayment $ ResourcePayment x
        AdditionalActionsCost -> do
          actionRemainingCount <- field InvestigatorRemainingActions iid
          let currentlyPaid = countAdditionalActionPayments (activeCostPayments c)
          c <$ if actionRemainingCount == 0
            then pure ()
            else push
              (chooseOne
                iid
                [ Label
                  "Spend 1 additional action"
                  [ PayCost
                    (InvestigatorSource iid)
                    iid
                    Nothing
                    skipAdditionalCosts
                    (ActionCost 1)
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
            else getActionCostModifier iid mAction
          let modifiedActionCost = max 0 (x + costModifier)
          push (SpendActions iid source modifiedActionCost)
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
        ClueCost x -> do
          push (InvestigatorSpendClues iid x)
          withPayment $ CluePayment x
        PlaceClueOnLocationCost x -> do
          push (InvestigatorPlaceCluesOnLocation iid x)
          withPayment $ CluePayment x
        GroupClueCost x locationMatcher -> do
          totalClues <- getPlayerCountValue x
          iids <-
            selectList
            $ InvestigatorAt locationMatcher
            <> InvestigatorWithAnyClues
          iidsWithClues <-
            filter ((> 0) . snd)
              <$> traverse (traverseToSnd (getSpendableClueCount . pure)) iids
          case iidsWithClues of
            [(iid', _)] ->
              c <$ push (PayCost source iid' mAction True (ClueCost totalClues))
            _ -> do
              let
                paymentOptions = map
                  (\(iid', clues) ->
                    ( iid'
                    , (0, clues)
                    , PayCost source iid' mAction True (ClueCost 1)
                    )
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
          let cards = filter (`cardMatch` cardMatcher) handCards
          push $ chooseN
            iid
            x
            [ TargetLabel
                (CardIdTarget $ toCardId card)
                [ PayCost
                    (InvestigatorSource iid)
                    iid
                    Nothing
                    skipAdditionalCosts
                    (DiscardCardCost $ PlayerCard card)
                ]
            | card <- cards
            ]
          pure c
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
                      (InvestigatorSource iid)
                      iid
                      Nothing
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
                (count (`member` insertSet SkillWild skillTypes)
                . cdSkills
                . toCardDef
                )
              )
              handCards
            cardMsgs = map
              (\(n, card) -> if n >= x
                then Run
                  [ DiscardCard iid (toCardId card)
                  , PaidAbilityCost
                    iid
                    Nothing
                    (SkillIconPayment $ cdSkills $ toCardDef card)
                  ]
                else Run
                  [ DiscardCard iid (toCardId card)
                  , PaidAbilityCost
                    iid
                    Nothing
                    (SkillIconPayment $ cdSkills $ toCardDef card)
                  , PayCost
                    source
                    iid
                    mAction
                    skipAdditionalCosts
                    (SkillIconCost (x - n) skillTypes)
                  ]
              )
              cards
          c <$ push (chooseOne iid cardMsgs)
        Free -> pure c
    PaidCost _ _ payment -> pure $ c & costPaymentsL <>~ payment
    PayCostFinished source iid -> do
      case activeCostTarget c of
        ForAbility ability -> do
          let action = fromMaybe Action.Ability (abilityAction ability)
          whenActivateAbilityWindow <- checkWindows
            [Window Timing.When (Window.ActivateAbility iid ability)]
          afterWindowMsgs <- checkWindows
            [Window Timing.After (Window.PerformAction iid action)]
          pushAll
            ([ whenActivateAbilityWindow
             , UseCardAbility iid source (activeCostWindows c) abilityIndex (activeCostPayments c)
             , ClearDiscardCosts
             ]
            <> [afterWindowMsgs, FinishAction]
            )
        _ -> pure ()
      pure c
    _ -> pure c
