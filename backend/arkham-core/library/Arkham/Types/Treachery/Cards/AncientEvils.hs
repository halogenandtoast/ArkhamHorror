module Arkham.Types.Treachery.Cards.AncientEvils where

import Arkham.Prelude

import qualified Arkham.Treachery.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype AncientEvils = AncientEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasActions)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientEvils :: TreacheryCard AncientEvils
ancientEvils = treachery AncientEvils Cards.ancientEvils

instance TreacheryRunner env => RunMessage env AncientEvils where
  runMessage msg t@(AncientEvils attrs@TreacheryAttrs {..}) = case msg of
    Revelation _ source | isSource attrs source -> do
      t <$ pushAll
        [ PlaceDoomOnAgenda
        , AdvanceAgendaIfThresholdSatisfied
        , Discard (TreacheryTarget treacheryId)
        ]
    _ -> AncientEvils <$> runMessage msg attrs
