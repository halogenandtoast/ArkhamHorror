module Arkham.Types.Effect.Effects.PayForAbilityEffect
  ( payForAbilityEffect
  , PayForAbilityEffect(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability, TakenAction)
import Arkham.Types.Action qualified as Action
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Id
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype PayForAbilityEffect = PayForAbilityEffect (EffectAttrs `With` Payment)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

payForAbilityEffect
  :: EffectId -> Ability -> Source -> Target -> [Window] -> PayForAbilityEffect
payForAbilityEffect eid ability source target windows' =
  PayForAbilityEffect $ (`with` NoPayment) $ EffectAttrs
    { effectId = eid
    , effectSource = source
    , effectTarget = target
    , effectCardCode = Nothing
    , effectMetadata = Just (EffectAbility (ability, windows'))
    , effectTraits = mempty
    , effectWindow = Nothing
    }

instance HasModifiersFor env PayForAbilityEffect where
  getModifiersFor _ target (PayForAbilityEffect (With EffectAttrs {..} _))
    | target == effectTarget = case effectMetadata of
      Just (EffectModifiers modifiers) -> pure modifiers
      _ -> pure []
  getModifiersFor _ _ _ = pure []

matchTarget :: [Action] -> ActionTarget -> Action -> Bool
matchTarget takenActions (FirstOneOf as) action =
  action `elem` as && all (`notElem` takenActions) as
matchTarget _ (IsAction a) action = action == a
matchTarget _ (EnemyAction a _) action = action == a

getActionCostModifier
  :: ( MonadReader env m
     , HasModifiersFor env ()
     , HasList Action.TakenAction env InvestigatorId
     )
  => InvestigatorId
  -> Maybe Action
  -> m Int
getActionCostModifier _ Nothing = pure 0
getActionCostModifier iid (Just a) = do
  takenActions <- map unTakenAction <$> getList iid
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
  :: (MonadReader env m, MonadIO m, HasQueue env)
  => InvestigatorId
  -> Window
  -> AbilityType
  -> Source
  -> Bool
  -> m ()
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
    ActionAbility mAction cost ->
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

instance
  ( HasQueue env
  , HasCostPayment env
  , HasSet Trait env Source
  , HasSet InvestigatorId env ()
  , HasModifiersFor env ()
  , HasCount ActionRemainingCount env InvestigatorId
  , HasId LeadInvestigatorId env ()
  )
  => RunMessage env PayForAbilityEffect where
  runMessage msg e@(PayForAbilityEffect (attrs `With` payments)) = case msg of
    CreatedEffect eid (Just (EffectAbility (Ability {..}, _))) source (InvestigatorTarget iid)
      | eid == toId attrs
      -> do
        push (PayAbilityCostFinished (toId attrs) source iid)
        e
          <$ startPayment
               iid
               (Window Timing.When Window.NonFast) -- TODO: a thing
               abilityType
               abilitySource
               abilityDoesNotProvokeAttacksOfOpportunity
    PayAbilityCost source iid mAction skipAdditionalCosts cost -> do
      let
        withPayment payment =
          pure $ PayForAbilityEffect (attrs `With` (payments <> payment))
      case cost of
        Costs xs ->
          e
            <$ pushAll
                 [ PayAbilityCost source iid mAction skipAdditionalCosts x
                 | x <- xs
                 ]
        UpTo 0 _ -> pure e
        UpTo n cost' -> do
          canAfford <- getCanAffordCost iid source mAction [] cost'
          e <$ when
            canAfford
            (push $ Ask iid $ ChoosePaymentAmounts
              ("Pay up to " <> displayCostType cost')
              Nothing
              [ ( iid
                , (0, n)
                , PayAbilityCost source iid mAction skipAdditionalCosts cost'
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
          e <$ push
            (chooseOne
              iid
              [ TargetLabel
                  target
                  [ PayAbilityCost
                      source
                      iid
                      mAction
                      skipAdditionalCosts
                      (ExhaustCost target)
                  ]
              | target <- targets
              ]
            )
        DiscardCost target -> do
          push (Discard target)
          withPayment $ DiscardPayment [target]
        DiscardCardCost card -> do
          push (DiscardCard iid (toCardId card))
          withPayment $ DiscardCardPayment [card]
        DiscardDrawnCardCost -> case effectMetadata attrs of
          Just (EffectAbility (_, windows')) -> do
            let
              getDrawnCard [] = error "can not find drawn card in windows"
              getDrawnCard (x : xs) = case x of
                Window _ (Window.DrawCard _ c _) -> c
                _ -> getDrawnCard xs
              card = getDrawnCard windows'
            push (DiscardCard iid (toCardId card))
            withPayment $ DiscardCardPayment [card]
          _ -> error "invalid metadata for ability"
        ExileCost target -> do
          push (Exile target)
          withPayment $ ExilePayment [target]
        RemoveCost target -> do
          push (RemoveFromGame target)
          withPayment $ RemovePayment [target]
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
              push (InvestigatorDirectDamage iid' source x 0)
              withPayment $ DirectDamagePayment x
            _ -> error "exactly one investigator expected for direct damage"
        ResourceCost x -> do
          push (SpendResources iid x)
          withPayment $ ResourcePayment x
        AdditionalActionsCost -> do
          actionRemainingCount <- unActionRemainingCount <$> getCount iid
          let currentlyPaid = countAdditionalActionPayments payments
          e <$ if actionRemainingCount == 0
            then pure ()
            else push
              (chooseOne
                iid
                [ Label
                  "Spend 1 additional action"
                  [ PayAbilityCost
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
        UseCost aid uType n -> do
          push (SpendUses (AssetTarget aid) uType n)
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
              <$> traverse
                    (traverseToSnd (fmap unSpendableClueCount . getCount))
                    iids
          case iidsWithClues of
            [(iid', _)] ->
              e
                <$ push
                     (PayAbilityCost
                       source
                       iid'
                       mAction
                       True
                       (ClueCost totalClues)
                     )
            _ -> do
              let
                paymentOptions = map
                  (\(iid', clues) ->
                    ( iid'
                    , (0, clues)
                    , PayAbilityCost source iid' mAction True (ClueCost 1)
                    )
                  )
                  iidsWithClues
              leadInvestigatorId <- getLeadInvestigatorId
              e <$ push
                (Ask leadInvestigatorId $ ChoosePaymentAmounts
                  (displayCostType cost)
                  (Just totalClues)
                  paymentOptions
                )
          -- push (SpendClues totalClues iids)
          -- withPayment $ CluePayment totalClues
        HandDiscardCost x mPlayerCardType traits skillTypes -> do
          handCards <- mapMaybe (preview _PlayerCard . unHandCard)
            <$> getList iid
          let
            cards = filter
              (and . sequence
                [ maybe (const True) (==) mPlayerCardType
                . cdCardType
                . toCardDef
                , (|| null traits) . notNull . intersection traits . toTraits
                , (|| null skillTypes)
                . not
                . null
                . intersection (insertSet SkillWild skillTypes)
                . setFromList
                . cdSkills
                . toCardDef
                ]
              )
              handCards
          e <$ push
            (chooseN
              iid
              x
              [ TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ PayAbilityCost
                      (InvestigatorSource iid)
                      iid
                      Nothing
                      skipAdditionalCosts
                      (DiscardCardCost $ PlayerCard card)
                  ]
              | card <- cards
              ]
            )
        DiscardFromCost x zone cardMatcher -> do
          let
            getCards = \case
              FromHandOf whoMatcher ->
                selectList (InHandOf whoMatcher <> BasicCardMatch cardMatcher)
              FromPlayAreaOf whoMatcher ->
                map unInPlayCard
                  <$> (selectList whoMatcher >>= concatMapM getList)
              CostZones zs -> concatMapM getCards zs
          cards <- getCards zone
          e <$ push
            (chooseN
              iid
              x
              [ TargetLabel
                  (CardIdTarget $ toCardId card)
                  [ PayAbilityCost
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
          handCards <- mapMaybe (preview _PlayerCard . unHandCard)
            <$> getList iid
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
                  , PayAbilityCost
                    source
                    iid
                    mAction
                    skipAdditionalCosts
                    (SkillIconCost (x - n) skillTypes)
                  ]
              )
              cards
          e <$ push (chooseOne iid cardMsgs)
        Free -> pure e
    PaidAbilityCost _ _ payment ->
      pure $ PayForAbilityEffect (attrs `with` (payments <> payment))
    PayAbilityCostFinished eid source iid | eid == toId attrs ->
      case effectMetadata attrs of
        Just (EffectAbility (ability@Ability {..}, windows')) -> do
          afterWindowMsgs <- case abilityAction ability of
            Just action -> checkWindows
              [Window Timing.After (Window.PerformAction iid action)]
            Nothing -> pure []
          e <$ pushAll
            ([ DisableEffect $ toId attrs
             , UseCardAbility iid source windows' abilityIndex payments
             ]
            <> afterWindowMsgs
            )
        _ -> e <$ push (DisableEffect $ toId attrs)
    _ -> PayForAbilityEffect . (`with` payments) <$> runMessage msg attrs
