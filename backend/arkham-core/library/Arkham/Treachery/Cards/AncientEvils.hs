module Arkham.Treachery.Cards.AncientEvils where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AncientEvils = AncientEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

ancientEvils :: TreacheryCard AncientEvils
ancientEvils = treachery AncientEvils Cards.ancientEvils

instance RunMessage AncientEvils where
  runMessage msg t@(AncientEvils attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      pushAll [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
      pure t
    _ -> AncientEvils <$> runMessage msg attrs
