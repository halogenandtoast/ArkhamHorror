module Arkham.Asset.Assets.EmpiricalHypothesis (empiricalHypothesis, empiricalHypothesisEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (Discarded)
import Arkham.Asset.Types (Asset, metaL)
import Arkham.Asset.Uses
import Arkham.Effect.Import
import Arkham.Helpers.Customization
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Taboo

newtype EmpiricalHypothesis = EmpiricalHypothesis AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empiricalHypothesis :: AssetCard EmpiricalHypothesis
empiricalHypothesis = assetWith EmpiricalHypothesis Cards.empiricalHypothesis (metaL .~ toJSON @[Int] [])

instance HasAbilities EmpiricalHypothesis where
  getAbilities (EmpiricalHypothesis a) =
    [ restricted a 1 ControlsThis $ forced $ RoundBegins #when
    , (if tabooed TabooList23 a then limited (PlayerLimit PerRound 2) else id)
        $ withTooltip "{fast} Spend 1 evidence: Draw 1 card."
        $ restricted a 2 (CanDrawCards <> exists matcher)
        $ FastAbility
        $ assetUseCost a Evidence 1
    ]
      <> [ withTooltip "{fast} Spend 2 evidence: Reduce the cost of the next card you play by 3."
            $ restricted a 3 (exists matcher)
            $ FastAbility
            $ assetUseCost a Evidence 2
         | a `hasCustomization` ResearchGrant
         ]
      <> [ withTooltip "{fast} Spend 3 evidence: Discover 1 clue at your location."
            $ restricted a 4 (CanDiscoverCluesAt YourLocation <> exists matcher)
            $ FastAbility
            $ assetUseCost a Evidence 3
         | a `hasCustomization` IrrefutableProof
         ]
      <> [ withTooltip
            "You may resolve its forced effect, choosing a criteria you have not chosen this round. Then, ready it."
            $ playerLimit PerWindow
            $ restricted a 5 (ControlsThis <> alternativeHypothesisCriteria)
            $ freeReaction
            $ Exhausts #after You
            $ TargetIs (toTarget a)
         | a `hasCustomization` AlternativeHypothesis
         ]
   where
    alternativeHypothesisCriteria =
      if any (`notElem` (toResult @[Int] $ assetMeta a)) availableOptions
        then NoRestriction
        else Never
    matcher =
      You <> case assetController a of
        Just iid ->
          if a `hasCustomization` PeerReview
            then colocatedWith iid
            else InvestigatorWithId iid
        Nothing -> NoOne
    availableOptions =
      [1, 2]
        <> [3 | a `hasCustomization` PessimisticOutlook]
        <> [4 | a `hasCustomization` TrialAndError]
        <> [5 | a `hasCustomization` IndepedentVariable]
        <> [6 | a `hasCustomization` FieldResearch]

instance RunMessage EmpiricalHypothesis where
  runMessage msg a@(EmpiricalHypothesis attrs) = runQueueT $ case msg of
    BeginRoundWindow -> do
      pure $ EmpiricalHypothesis $ attrs & metaL .~ toJSON @[Int] []
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let option n txt = labeled txt $ push $ HandleAbilityOption iid (toSource attrs) n
      chooseOneM iid do
        option 1 "You fail a test by 2 or more."
        option 2 "You succeed at a test by 3 or more."
        when (attrs `hasCustomization` PessimisticOutlook) do
          option 3 "You run out of cards in your hand."
        when (attrs `hasCustomization` TrialAndError) do
          option 4 "You are dealt damage or horror."
        when (attrs `hasCustomization` IndepedentVariable) do
          option 5 "You discard a treachery or enemy from play."
        when (attrs `hasCustomization` FieldResearch) do
          option 6 "You enter a location with 3 or more shroud."
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      drawCardsIfCan iid (attrs.ability 2) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      createCardEffect Cards.empiricalHypothesis Nothing attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 4 -> do
      discoverAtYourLocation NotInvestigate iid (toAbilitySource attrs 4) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 5 -> do
      let meta = toResult @[Int] (assetMeta attrs)
      let
        option n txt =
          unless (n `elem` meta) do
            labeled txt $ push $ HandleAbilityOption iid (toSource attrs) n
      chooseOneM iid do
        option 1 "You fail a test by 2 or more."
        option 2 "You succeed at a test by 3 or more."

        when (attrs `hasCustomization` PessimisticOutlook) do
          option 3 "You run out of cards in your hand."

        when (attrs `hasCustomization` TrialAndError) do
          option 4 "You are dealt damage or horror."

        when (attrs `hasCustomization` IndepedentVariable) do
          option 5 "You discard a treachery or enemy from play."

        when (attrs `hasCustomization` FieldResearch) do
          option 6 "You enter a location with 3 or more shroud."

      readyThis attrs
      pure a
    HandleAbilityOption iid (isSource attrs -> True) n -> do
      let meta = toResult (assetMeta attrs)
      createCardEffect Cards.empiricalHypothesis (Just $ EffectInt n) attrs iid
      pure $ EmpiricalHypothesis $ attrs & metaL .~ toJSON (meta <> [n])
    _ -> EmpiricalHypothesis <$> liftRunMessage msg attrs

data EmpiricalHypothesisEffectMetadata = EmpiricalHypothesisEffectMetadata
  { oldHandCount :: Int
  , pessimisticOutlook :: Bool
  , peerReview :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EmpiricalHypothesisEffect = EmpiricalHypothesisEffect EffectAttrs
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empiricalHypothesisEffect :: EffectArgs -> EmpiricalHypothesisEffect
empiricalHypothesisEffect =
  cardEffectWith
    EmpiricalHypothesisEffect
    Cards.empiricalHypothesis
    (extraL .~ toJSON (EmpiricalHypothesisEffectMetadata 0 False False))

instance HasModifiersFor EmpiricalHypothesisEffect where
  getModifiersFor (EmpiricalHypothesisEffect a) =
    if isNothing a.meta then modified_ a a.target [ReduceCostOf AnyCard 3] else pure mempty

instance HasAbilities EmpiricalHypothesisEffect where
  getAbilities (EmpiricalHypothesisEffect attrs) | isJust attrs.meta = case attrs.source of
    AssetSource aid ->
      [ withTooltip
          ("When " <> trigger <> ", you may exhaust Empirical Hypothesis to add 1 evidence to it.")
          $ restrictedAbility (proxied (AssetWithId aid) attrs) 1 criteria
          $ ReactionAbility reactionWindow (exhaust aid)
      ]
    _ -> error $ "invalid effect source: " <> show attrs.source
   where
    trigger = case attrs.meta of
      Just (EffectInt 1) -> "you fail a test by 2 or more."
      Just (EffectInt 2) -> "you succeed at a test by 3 or more."
      Just (EffectInt 3) -> "you run out of cards in your hand."
      Just (EffectInt 4) -> "you are dealt damage or horror."
      Just (EffectInt 5) -> "you discard a treachery or enemy from play."
      Just (EffectInt 6) -> "you enter a location with 3 or more shroud."
      _ -> error $ "invalid effect meta: " <> show attrs.meta
    matcher = case attrs.target of
      InvestigatorTarget iid ->
        if peerReview (toResult attrs.extra)
          then colocatedWith iid
          else InvestigatorWithId iid
      _ -> error $ "invalid effect target: " <> show attrs.target
    criteria = case attrs.target of
      InvestigatorTarget iid -> exists (You <> InvestigatorWithId iid)
      _ -> error $ "invalid effect target: " <> show attrs.target
    reactionWindow = case attrs.meta of
      Just (EffectInt 1) -> SkillTestResult #when matcher AnySkillTest $ FailureResult $ atLeast 2
      Just (EffectInt 2) -> SkillTestResult #when matcher AnySkillTest $ SuccessResult $ atLeast 3
      Just (EffectInt 3) ->
        if pessimisticOutlook (toResult attrs.extra)
          then AnyWindow
          else NotAnyWindow
      Just (EffectInt 4) -> DealtDamageOrHorror #when AnySource matcher
      Just (EffectInt 5) ->
        oneOf
          [ EnemyDiscarded #when AnySource (EnemyDiscardedBy You)
          , TreacheryDiscarded #when AnySource (InPlayTreachery <> TreacheryDiscardedBy You)
          ]
      Just (EffectInt 6) -> Enters #after matcher $ LocationWithShroud $ atLeast 3 -- NB. We use after so the location is revealed
      _ -> error $ "invalid effect: " <> show attrs.meta
  getAbilities _ = []

-- TODO: Determine if this is a delayed effect, lasting effect, or replaced
-- Currently treating is as though it is replaced, if this switchings to
-- delayed: then we would disable on the AddUses line and remove the HandleAbilityOption
-- lasting: remove the HandleAbilityOption
instance RunMessage EmpiricalHypothesisEffect where
  runMessage msg e@(EmpiricalHypothesisEffect attrs) = runQueueT $ case msg of
    CreatedEffect eid _ (AssetSource aid) _ | eid == toId attrs -> do
      peerReview <- getHasCustomization @Asset aid PeerReview
      EmpiricalHypothesisEffect
        <$> liftRunMessage
          msg
          (attrs & extraL .~ toJSON (EmpiricalHypothesisEffectMetadata 0 False peerReview))
    UseThisAbility _ (ProxySource _ (isSource attrs -> True)) 1 -> do
      case attrs.source.asset of
        Just aid -> push $ AddUses attrs.source aid Evidence 1
        _ -> error $ "invalid effect source: " <> show attrs.source
      pure e
    EndRound | isJust attrs.meta -> do
      disableReturn e
    CardEnteredPlay iid _ | isNothing attrs.meta -> do
      when (toTarget iid == attrs.target) (disable attrs)
      pure e
    Do (CheckWindows {}) | isJust attrs.meta -> do
      attrs' <- liftRunMessage msg attrs
      case attrs.target of
        InvestigatorTarget iid -> do
          let extra = toResult attrs.extra
          newCount <- fieldMap InvestigatorHand length iid
          let pessimistic = oldHandCount extra > 0 && newCount == 0
          let newExtra = toJSON $ extra {oldHandCount = newCount, pessimisticOutlook = pessimistic}
          pure $ EmpiricalHypothesisEffect $ attrs' & extraL .~ newExtra
        _ -> pure $ EmpiricalHypothesisEffect attrs'
    HandleAbilityOption _ source _ | source == attrs.source -> do
      disableReturn e
    _ -> EmpiricalHypothesisEffect <$> liftRunMessage msg attrs
