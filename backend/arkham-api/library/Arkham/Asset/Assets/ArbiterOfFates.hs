module Arkham.Asset.Assets.ArbiterOfFates (arbiterOfFates, arbiterOfFatesEffect, ArbiterOfFates (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype ArbiterOfFates = ArbiterOfFates AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arbiterOfFates :: AssetCard ArbiterOfFates
arbiterOfFates =
  asset ArbiterOfFates Cards.arbiterOfFates

instance HasModifiersFor ArbiterOfFates where
  getModifiersFor (ArbiterOfFates a) =
    if a.exhausted
      then pure mempty
      else
        selectOne (AbilityIs (InvestigatorSource "60401") 1) >>= \case
          Just ab -> modified_ a (AbilityTarget "60401" ab) [CanIgnoreLimit]
          Nothing -> pure mempty

instance HasAbilities ArbiterOfFates where
  getAbilities (ArbiterOfFates a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (ActivateAbility #when You $ AbilityIs (InvestigatorSource "60401") 1)
          (ExhaustCost $ toTarget a)
    ]

getAbility :: [Window] -> Ability
getAbility [] = error "invalid ability"
getAbility ((windowType -> Window.ActivateAbility _ _ ab) : _) = ab
getAbility (_ : xs) = getAbility xs

instance RunMessage ArbiterOfFates where
  runMessage msg a@(ArbiterOfFates attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAbility -> ab) _ -> do
      push $ DoNotCountUseTowardsAbilityLimit "60401" ab
      createCardEffect Cards.arbiterOfFates Nothing attrs iid
      pure a
    _ -> ArbiterOfFates <$> liftRunMessage msg attrs

newtype ArbiterOfFatesEffect = ArbiterOfFatesEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arbiterOfFatesEffect :: EffectArgs -> ArbiterOfFatesEffect
arbiterOfFatesEffect = cardEffect ArbiterOfFatesEffect Cards.arbiterOfFates

instance HasModifiersFor ArbiterOfFatesEffect where
  getModifiersFor (ArbiterOfFatesEffect a) =
    if a.finished
      then pure mempty
      else
        selectOne (AbilityIs (InvestigatorSource "60401") 1) >>= \case
          Just ab -> modified_ a (AbilityTarget "60401" ab) [IgnoreLimit]
          Nothing -> pure mempty

-- > When you use Jacqueline Fine's  ability, exhaust Arbiter of Fates: This use of her ability does not count towards its limit.
-- We basically track if the ability is done being used and then disable this effect

instance RunMessage ArbiterOfFatesEffect where
  runMessage msg e@(ArbiterOfFatesEffect attrs) = runQueueT $ case msg of
    UseThisAbility "60401" (InvestigatorSource "60401") 1 -> do
      pure . ArbiterOfFatesEffect $ finishedEffect attrs
    ResolvedAbility ab | ab.source == InvestigatorSource "60401" && ab.index == 1 -> disableReturn e
    _ -> ArbiterOfFatesEffect <$> liftRunMessage msg attrs
