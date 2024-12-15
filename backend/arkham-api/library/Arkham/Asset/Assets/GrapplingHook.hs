module Arkham.Asset.Assets.GrapplingHook (grapplingHook, grapplingHookEffect, GrapplingHook (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card.CardCode
import Arkham.Effect.Import
import Arkham.Effect.Types (EffectBuilder (..), makeEffectBuilder)
import Arkham.Helpers.Modifiers (ModifierType (..), modifiedWhen_)
import Arkham.Helpers.SkillTest (getSkillTestAction)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

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
        $ restricted x 1 ControlsThis
        $ ActionAbility [] (exhaust x <> ActionCost 2)
    ]

instance RunMessage GrapplingHook where
  runMessage msg a@(GrapplingHook (With attrs meta)) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      doStep 3 msg
      pure . GrapplingHook $ attrs `with` Metadata []
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      abilities' <-
        selectMap DifferentAbility
          $ PerformableAbility [ActionCostModifier (-1)]
          <> BasicAbility
          <> mapOneOf AbilityIsAction [#engage, #evade, #investigate, #move]
      effectId <- getRandom
      builder <- makeEffectBuilder (toCardCode attrs) Nothing (attrs.ability 1) iid
      chooseOrRunOneM iid do
        labeled "Take no more actions" nothing
        for_ (filter (`notElem` chosenAbilities meta) abilities') \(DifferentAbility ab) -> do
          if #investigate `elem` ab.actions
            then do
              abilityLabeledWithBefore
                iid
                (overCost (`decreaseActionCost` 1) $ doesNotProvokeAttacksOfOpportunity ab)
                [CreateEffect builder {effectBuilderEffectId = Just effectId}]
                do
                  handleTarget iid attrs (AbilityTarget iid ab)
                  disable effectId
                  doStep (n - 1) msg'
            else do
              abilityLabeled iid (overCost (`decreaseActionCost` 1) $ doesNotProvokeAttacksOfOpportunity ab) do
                handleTarget iid attrs (AbilityTarget iid ab)
                doStep (n - 1) msg'
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
  getModifiersFor (GrapplingHookEffect a) = do
    valid <- (== Just #investigate) <$> getSkillTestAction
    modifiedWhen_ a valid a.target [UseSkillInsteadOf #intellect #agility]

instance RunMessage GrapplingHookEffect where
  runMessage msg (GrapplingHookEffect attrs) =
    GrapplingHookEffect <$> runMessage msg attrs
