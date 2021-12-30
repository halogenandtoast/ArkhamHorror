module Arkham.Treachery.Cards.AncientEvils where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Classes
import Arkham.Message
import Arkham.Treachery.Attrs
import Arkham.Treachery.Runner

newtype AncientEvils = AncientEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientEvils :: TreacheryCard AncientEvils
ancientEvils = treachery AncientEvils Cards.ancientEvils

instance TreacheryRunner env => RunMessage env AncientEvils where
  runMessage msg t@(AncientEvils attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      t <$ pushAll [PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
    _ -> AncientEvils <$> runMessage msg attrs
