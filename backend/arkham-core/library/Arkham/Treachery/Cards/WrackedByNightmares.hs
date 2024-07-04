module Arkham.Treachery.Cards.WrackedByNightmares (WrackedByNightmares (..), wrackedByNightmares) where

import Arkham.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype WrackedByNightmares = WrackedByNightmares TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wrackedByNightmares :: TreacheryCard WrackedByNightmares
wrackedByNightmares = treachery WrackedByNightmares Cards.wrackedByNightmares

instance HasModifiersFor WrackedByNightmares where
  getModifiersFor (InvestigatorTarget iid) (WrackedByNightmares attrs) = do
    modified attrs [ControlledAssetsCannotReady | treacheryInThreatArea iid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities WrackedByNightmares where
  getAbilities (WrackedByNightmares a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectEach (assetControlledBy iid) $ push . Exhaust . toTarget
      placeInThreatArea attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> WrackedByNightmares <$> liftRunMessage msg attrs
