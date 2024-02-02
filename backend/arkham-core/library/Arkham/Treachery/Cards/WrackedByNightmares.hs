module Arkham.Treachery.Cards.WrackedByNightmares (WrackedByNightmares (..), wrackedByNightmares) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Matcher
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Helpers
import Arkham.Treachery.Runner

newtype WrackedByNightmares = WrackedByNightmares TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

wrackedByNightmares :: TreacheryCard WrackedByNightmares
wrackedByNightmares = treachery WrackedByNightmares Cards.wrackedByNightmares

instance HasModifiersFor WrackedByNightmares where
  getModifiersFor (InvestigatorTarget iid) (WrackedByNightmares attrs) = do
    pure $ toModifiers attrs [ControlledAssetsCannotReady | treacheryOnInvestigator iid attrs]
  getModifiersFor _ _ = pure []

instance HasAbilities WrackedByNightmares where
  getAbilities (WrackedByNightmares a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage WrackedByNightmares where
  runMessage msg t@(WrackedByNightmares attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      assets <- selectList $ assetControlledBy iid
      pushAll $ map (Exhaust . toTarget) assets <> [attachTreachery attrs iid]
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toId attrs)
      pure t
    _ -> WrackedByNightmares <$> runMessage msg attrs
