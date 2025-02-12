module Arkham.Treachery.Cards.IllDoItMyself (illDoItMyself) where

import Arkham.Ability
import Arkham.Placement
import Arkham.Helpers.Modifiers (inThreatAreaGets, ModifierType(..))
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype IllDoItMyself = IllDoItMyself TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

illDoItMyself :: TreacheryCard IllDoItMyself
illDoItMyself = treachery IllDoItMyself Cards.illDoItMyself

instance HasModifiersFor IllDoItMyself where
  getModifiersFor (IllDoItMyself a) = case a.placement of
    InThreatArea iid ->
      inThreatAreaGets
        a
        [AdditionalPlayCostOf (basic #event) (HorrorCost (toSource a) (toTarget iid) 1)]
    _ -> pure ()

instance HasAbilities IllDoItMyself where
  getAbilities (IllDoItMyself a) =
    [restricted a 1 OnSameLocation $ FastAbility $ HandDiscardCost 3 (basic #event)]

instance RunMessage IllDoItMyself where
  runMessage msg t@(IllDoItMyself attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> IllDoItMyself <$> liftRunMessage msg attrs
