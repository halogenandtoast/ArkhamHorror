module Arkham.Treachery.Cards.DeeperSlumber (deeperSlumber, DeeperSlumber (..)) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype DeeperSlumber = DeeperSlumber TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deeperSlumber :: TreacheryCard DeeperSlumber
deeperSlumber = treachery DeeperSlumber Cards.deeperSlumber

instance HasModifiersFor DeeperSlumber where
  getModifiersFor (InvestigatorTarget iid) (DeeperSlumber attrs) | treacheryInThreatArea iid attrs = do
    modified attrs [HandSize (-3), CheckHandSizeAfterDraw]
  getModifiersFor _ _ = pure []

instance HasAbilities DeeperSlumber where
  getAbilities (DeeperSlumber a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage DeeperSlumber where
  runMessage msg t@(DeeperSlumber attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> DeeperSlumber <$> liftRunMessage msg attrs
