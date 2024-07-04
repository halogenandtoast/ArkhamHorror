module Arkham.Treachery.Cards.LawOfYgirothChaos (lawOfYgirothChaos, LawOfYgirothChaos (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher hiding (TreacheryInHandOf, treacheryInHandOf)
import Arkham.Placement
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LawOfYgirothChaos = LawOfYgirothChaos TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lawOfYgirothChaos :: TreacheryCard LawOfYgirothChaos
lawOfYgirothChaos = treachery LawOfYgirothChaos Cards.lawOfYgirothChaos

instance HasModifiersFor LawOfYgirothChaos where
  getModifiersFor (InvestigatorTarget iid) (LawOfYgirothChaos a) | treacheryInHandOf a == Just iid = do
    modified
      a
      [CannotPlay CardWithOddCost, CannotTriggerAbilityMatching $ AbilityOnCard CardWithOddCost]
  getModifiersFor _ _ = pure []

instance HasAbilities LawOfYgirothChaos where
  getAbilities (LawOfYgirothChaos a) =
    [restrictedAbility a 1 InYourHand $ actionAbilityWithCost (HandDiscardCost 1 CardWithEvenCost)]

instance RunMessage LawOfYgirothChaos where
  runMessage msg t@(LawOfYgirothChaos attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeTreachery attrs (HiddenInHand iid)
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) (toTarget attrs)
      pure t
    _ -> LawOfYgirothChaos <$> liftRunMessage msg attrs
