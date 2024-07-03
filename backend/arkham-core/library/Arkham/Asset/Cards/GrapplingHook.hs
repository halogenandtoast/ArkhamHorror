module Arkham.Asset.Cards.GrapplingHook (grapplingHook, grapplingHookEffect, GrapplingHook (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Matcher
import Arkham.Modifier

newtype Metadata = Metadata {chosenAbilities :: [DifferentAbility]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype GrapplingHook = GrapplingHook (AssetAttrs `With` Metadata)
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grapplingHook :: AssetCard GrapplingHook
grapplingHook = asset (GrapplingHook . (`with` Metadata [])) Cards.grapplingHook

instance HasAbilities GrapplingHook where
  getAbilities (GrapplingHook (With x _)) =
    [ doesNotProvokeAttacksOfOpportunity
        $ restrictedAbility x 1 ControlsThis
        $ ActionAbility []
        $ exhaust x
        <> ActionCost 2
    ]

instance RunMessage GrapplingHook where
  runMessage msg a@(GrapplingHook (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      createCardEffect Cards.grapplingHook Nothing (attrs.ability 1) iid
      push $ DoStep 3 msg
      pure a
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      abilities' <-
        selectMap DifferentAbility
          $ PerformableAbility [ActionCostModifier (-1)]
          <> BasicAbility
          <> oneOf (map AbilityIsAction [#engage, #evade, #investigate, #move])
      chooseOrRunOne iid $ Label "Take no more actions" []
        : [ AbilityLabel
            iid
            (overCost (`decreaseActionCost` 1) $ doesNotProvokeAttacksOfOpportunity ab)
            []
            [HandleTargetChoice iid (toSource attrs) (AbilityTarget iid ab), DoStep (n - 1) msg']
          | DifferentAbility ab <- filter (`notElem` chosenAbilities meta) (traceShowId abilities')
          ]
      pure a
    HandleTargetChoice _ (isSource attrs -> True) (AbilityTarget _ ab) -> do
      pure . GrapplingHook $ attrs `with` Metadata (DifferentAbility ab : chosenAbilities meta)
    _ -> GrapplingHook . (`with` meta) <$> liftRunMessage msg attrs

newtype GrapplingHookEffect = GrapplingHookEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grapplingHookEffect :: EffectArgs -> GrapplingHookEffect
grapplingHookEffect = cardEffect GrapplingHookEffect Cards.grapplingHook

instance HasModifiersFor GrapplingHookEffect where
  getModifiersFor target (GrapplingHookEffect a) | isTarget a.target target = do
    valid <- (== Just #investigate) <$> getSkillTestAction
    modified a [UseSkillInsteadOf #agility #intellect | valid]
  getModifiersFor _ _ = pure []

instance RunMessage GrapplingHookEffect where
  runMessage msg e@(GrapplingHookEffect attrs) = runQueueT $ case msg of
    ResolvedAbility ab | AbilitySource ab.source ab.index == attrs.source -> disableReturn e
    _ -> GrapplingHookEffect <$> liftRunMessage msg attrs
