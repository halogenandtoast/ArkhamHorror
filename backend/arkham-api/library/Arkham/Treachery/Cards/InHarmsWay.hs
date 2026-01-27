module Arkham.Treachery.Cards.InHarmsWay (inHarmsWay) where

import Arkham.Ability
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype InHarmsWay = InHarmsWay TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inHarmsWay :: TreacheryCard InHarmsWay
inHarmsWay = treachery InHarmsWay Cards.inHarmsWay

instance HasAbilities InHarmsWay where
  getAbilities (InHarmsWay a) =
    restricted
      a
      1
      InYourThreatArea
      (forced $ EnemyDealtDamage #after AnyDamageEffect AnyEnemy (SourceOwnedBy You))
      : [mkAbility a 2 $ forcedOnElimination iid | iid <- toList a.inThreatAreaOf]

instance RunMessage InHarmsWay where
  runMessage msg t@(InHarmsWay attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      placeTokens (attrs.ability 1) attrs #damage 1
      doStep 1 msg
      pure t
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      when (attrs.token #damage >= 3) $ toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sufferPhysicalTrauma iid 1
      pure t
    _ -> InHarmsWay <$> liftRunMessage msg attrs
