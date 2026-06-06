module Arkham.Treachery.Cards.StickyFeet (stickyFeet) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modified_)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Token qualified as Token
import Arkham.Trait (Trait (Oozified))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype StickyFeet = StickyFeet TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

stickyFeet :: TreacheryCard StickyFeet
stickyFeet = treachery StickyFeet Cards.stickyFeet

instance HasModifiersFor StickyFeet where
  getModifiersFor (StickyFeet attrs) = case attrs.placement of
    AttachedToLocation lid ->
      modified_
        attrs
        lid
        [AdditionalCostToEnter (ActionCost 1), AdditionalCostToLeave (ActionCost 1)]
    _ -> pure mempty

instance HasAbilities StickyFeet where
  getAbilities (StickyFeet a) =
    [mkAbility a 1 $ actionAbilityWithCost (SpendTokenCost Token.Resource (TargetIs ScenarioTarget))]

instance RunMessage StickyFeet where
  runMessage msg t@(StickyFeet attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      locations <- select $ NearestLocationTo iid (LocationWithTrait Oozified)
      if null locations
        then gainSurge attrs
        else chooseOrRunOneM iid $ targets locations (attachTreachery attrs)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> StickyFeet <$> liftRunMessage msg attrs
