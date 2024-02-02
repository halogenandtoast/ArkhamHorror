module Arkham.Treachery.Cards.DeeperSlumber (
  deeperSlumber,
  DeeperSlumber (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype DeeperSlumber = DeeperSlumber TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

deeperSlumber :: TreacheryCard DeeperSlumber
deeperSlumber = treachery DeeperSlumber Cards.deeperSlumber

instance HasModifiersFor DeeperSlumber where
  getModifiersFor (InvestigatorTarget iid) (DeeperSlumber attrs) | treacheryOnInvestigator iid attrs = do
    pure $ toModifiers attrs [HandSize (-3), CheckHandSizeAfterDraw]
  getModifiersFor _ _ = pure []

instance HasAbilities DeeperSlumber where
  getAbilities (DeeperSlumber a) = [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage DeeperSlumber where
  runMessage msg t@(DeeperSlumber attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      push $ attachTreachery attrs iid
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ toDiscardBy iid (toAbilitySource attrs 1) (toId attrs)
      pure t
    _ -> DeeperSlumber <$> runMessage msg attrs
