module Arkham.Asset.Cards.EmpiricalHypothesis (
  empiricalHypothesis,
  empiricalHypothesisEffect,
  EmpiricalHypothesis (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (Discarded)
import Arkham.Discover
import Arkham.Effect.Runner hiding (Discarded)
import Arkham.Helpers.Customization
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype EmpiricalHypothesis = EmpiricalHypothesis AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empiricalHypothesis :: AssetCard EmpiricalHypothesis
empiricalHypothesis = assetWith EmpiricalHypothesis Cards.empiricalHypothesis (metaL .~ toJSON @[Int] [])

instance HasAbilities EmpiricalHypothesis where
  getAbilities (EmpiricalHypothesis a) =
    [ restrictedAbility a 1 ControlsThis $ ForcedAbility $ RoundBegins #when
    , withTooltip "{fast} Spend 1 evidence: Draw 1 card."
        $ restrictedAbility a 2 (CanDrawCards <> exists matcher)
        $ FastAbility
        $ assetUseCost a Evidence 1
    ]
      <> [ withTooltip "{fast} Spend 2 evidence: Reduce the cost of the next card you play by 3."
          $ restrictedAbility a 3 (exists matcher)
          $ FastAbility
          $ assetUseCost a Evidence 2
         | a `hasCustomization` ResearchGrant
         ]
      <> [ withTooltip "{fast} Spend 3 evidence: Discover 1 clue at your location."
          $ restrictedAbility a 4 (CanDiscoverCluesAt YourLocation <> exists matcher)
          $ FastAbility
          $ assetUseCost a Evidence 3
         | a `hasCustomization` IrrefutableProof
         ]
      <> [ withTooltip
          "You may resolve its forced effect, choosing a criteria you have not chosen this round. Then, ready it."
          $ playerLimit PerWindow
          $ restrictedAbility a 5 (ControlsThis <> alternativeHypothesisCriteria)
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
    matcher = case assetController a of
      Just iid ->
        if a `hasCustomization` PeerReview
          then colocatedWith iid
          else You <> InvestigatorWithId iid
      Nothing -> NoOne
    availableOptions =
      [1, 2]
        <> [3 | a `hasCustomization` PessimisticOutlook]
        <> [4 | a `hasCustomization` TrialAndError]
        <> [5 | a `hasCustomization` IndepedentVariable]
        <> [6 | a `hasCustomization` FieldResearch]

instance RunMessage EmpiricalHypothesis where
  runMessage msg a@(EmpiricalHypothesis attrs) = case msg of
    BeginRoundWindow -> do
      pure $ EmpiricalHypothesis $ attrs & metaL .~ toJSON @[Int] []
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      player <- getPlayer iid
      let option n = HandleAbilityOption iid (toSource attrs) n
      push
        $ chooseOne
          player
        $ [ Label "You fail a test by 2 or more." [option 1]
          , Label "You succeed at a test by 3 or more." [option 2]
          ]
        <> [ Label "You run out of cards in your hand." [option 3] | attrs `hasCustomization` PessimisticOutlook
           ]
        <> [ Label "You are dealt damage or horror." [option 4] | attrs `hasCustomization` TrialAndError
           ]
        <> [ Label "You discard a treachery or enemy from play." [option 5]
           | attrs `hasCustomization` IndepedentVariable
           ]
        <> [ Label "You enter a location with 3 or more shroud." [option 6]
           | attrs `hasCustomization` FieldResearch
           ]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      pushM $ drawCards iid (toAbilitySource attrs 2) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 3 -> do
      push $ createCardEffect Cards.empiricalHypothesis Nothing attrs iid
      pure a
    UseThisAbility iid (isSource attrs -> True) 4 -> do
      push $ toMessage $ discoverAtYourLocation iid (toAbilitySource attrs 4) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 5 -> do
      let meta = toResult @[Int] (assetMeta attrs)
      let option n = HandleAbilityOption iid (toSource attrs) n
      player <- getPlayer iid
      pushAll
        [ chooseOne
            player
            $ [Label "You fail a test by 2 or more." [option 1] | 1 `notElem` meta]
            <> [Label "You succeed at a test by 3 or more." [option 2] | 2 `notElem` meta]
            <> [ Label "You run out of cards in your hand." [option 3]
               | attrs `hasCustomization` PessimisticOutlook && 3 `notElem` meta
               ]
            <> [ Label "You are dealt damage or horror." [option 4]
               | attrs `hasCustomization` TrialAndError && 4 `notElem` meta
               ]
            <> [ Label "You discard a treachery or enemy from play." [option 5]
               | attrs `hasCustomization` IndepedentVariable && 5 `notElem` meta
               ]
            <> [ Label "You enter a location with 3 or more shroud." [option 6]
               | attrs `hasCustomization` FieldResearch && 6 `notElem` meta
               ]
        , Ready (toTarget attrs)
        ]
      pure a
    HandleAbilityOption iid (isSource attrs -> True) n -> do
      let meta = toResult (assetMeta attrs)
      push $ createCardEffect Cards.empiricalHypothesis (Just $ EffectInt n) attrs iid
      pure $ EmpiricalHypothesis $ attrs & metaL .~ toJSON (meta <> [n])
    _ -> EmpiricalHypothesis <$> runMessage msg attrs

data EmpiricalHypothesisEffectMetadata = EmpiricalHypothesisEffectMetadata
  { oldHandCount :: Int
  , pessimisticOutlook :: Bool
  , peerReview :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype EmpiricalHypothesisEffect = EmpiricalHypothesisEffect EffectAttrs
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empiricalHypothesisEffect :: EffectArgs -> EmpiricalHypothesisEffect
empiricalHypothesisEffect =
  cardEffectWith
    EmpiricalHypothesisEffect
    Cards.empiricalHypothesis
    (extraL .~ toJSON (EmpiricalHypothesisEffectMetadata 0 False False))

instance HasModifiersFor EmpiricalHypothesisEffect where
  getModifiersFor target (EmpiricalHypothesisEffect attrs) | isNothing attrs.meta && target == attrs.target = do
    pure $ toModifiers attrs [ReduceCostOf AnyCard 3]
  getModifiersFor _ _ = pure []

instance HasAbilities EmpiricalHypothesisEffect where
  getAbilities (EmpiricalHypothesisEffect attrs) | isJust attrs.meta = case attrs.source of
    AssetSource aid ->
      [ withTooltip
          ("When " <> trigger <> ", you may exhaust Empirical Hypothesis to add 1 evidence to it.")
          $ restrictedAbility (proxy (AssetWithId aid) attrs) 1 criteria
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
      Just (EffectInt 6) -> Enters #when matcher $ LocationWithShroud $ atLeast 3
      _ -> error $ "invalid effect: " <> show attrs.meta
  getAbilities _ = []

-- TODO: Determine if this is a delayed effect, lasting effect, or replaced
-- Currently treating is as thought it is replaced, if this switchings to
-- delayed: then we would disable on the AddUses line and remove the HandleAbilityOption
-- lasting: remove the HandleAbilityOption
instance RunMessage EmpiricalHypothesisEffect where
  runMessage msg e@(EmpiricalHypothesisEffect attrs) = case msg of
    CreatedEffect eid _ (AssetSource aid) _ | eid == toId attrs -> do
      peerReview <- getHasCustomization aid PeerReview
      EmpiricalHypothesisEffect
        <$> runMessage
          msg
          (attrs {effectExtraMetadata = toJSON $ EmpiricalHypothesisEffectMetadata 0 False peerReview})
    UseThisAbility _ (ProxySource _ (isSource attrs -> True)) 1 -> do
      case attrs.source of
        AssetSource aid -> push $ AddUses aid Evidence 1
        _ -> error $ "invalid effect source: " <> show attrs.source
      pure e
    EndRound | isJust attrs.meta -> do
      push $ disable attrs
      pure e
    CardEnteredPlay iid _ | isNothing attrs.meta -> do
      pushWhen (toTarget iid == attrs.target) (disable attrs)
      pure e
    RunWindow {} | isJust attrs.meta -> do
      attrs' <- runMessage msg attrs
      case attrs.target of
        InvestigatorTarget iid -> do
          let extra = toResult attrs.extra
          newCount <- fieldMap InvestigatorHand length iid
          let pessimistic = oldHandCount extra > 0 && newCount == 0
          let newExtra = toJSON $ extra {oldHandCount = newCount, pessimisticOutlook = pessimistic}
          pure $ EmpiricalHypothesisEffect $ attrs' {effectExtraMetadata = newExtra}
        _ -> pure $ EmpiricalHypothesisEffect attrs'
    HandleAbilityOption _ source _ | source == attrs.source -> do
      push $ disable attrs
      pure e
    _ -> EmpiricalHypothesisEffect <$> runMessage msg attrs
